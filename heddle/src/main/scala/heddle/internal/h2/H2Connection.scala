package heddle.internal.h2

import heddle.error.HttpError
import heddle.http.{Body, HttpVersion, Method, Request, Response, Url}
import heddle.http.header.{Header, Headers}
import heddle.internal.engine.ConnBuf
import heddle.route.Routes
import heddle.Server
import heddle.server.Http2Config
import zio.*
import zio.stream.ZStream

private[heddle] object H2Connection:
  def serve[R](
      routes: Routes[R, Response],
      src: ConnBuf,
      send: Chunk[Byte] => Task[Unit],
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean = false,
  ): ZIO[R, HttpError, Unit] =
    val h2 = config.http2Config
    for
      out     <- Queue.bounded[H2Frame](h2.maxOutstandingFrames)
      flow    <- H2Flow.make(h2.initialWindowSize.toInt)
      streams <- ZIO.succeed(java.util.concurrent.atomic.AtomicInteger(0))
      fibers  <- ZIO.succeed(java.util.concurrent.ConcurrentHashMap.newKeySet[Fiber[Any, Any]]())
      _       <- write(send, H2Frame.Settings(ack = false, localSettings(h2)))
      wf      <- writer(out, send).forkDaemon
      _       <- (reader(routes, src, out, flow, streams, fibers, config, takingWork, busy, secure) *>
        out.offer(H2Frame.GoAway(0, 0x0)).unit)
        .ensuring(
          interruptAll(fibers) *> out.shutdown *> wf.interrupt
        )
    yield ()
    end for
  end serve

  private def interruptAll(fibers: java.util.Set[Fiber[Any, Any]]): UIO[Unit] =
    ZIO.suspendSucceed {
      val it = fibers.iterator()
      val fs = scala.collection.mutable.ArrayBuffer.empty[Fiber[Any, Any]]
      while it.hasNext do fs += it.next()
      ZIO.foreachParDiscard(fs)(_.interrupt)
    }

  private def localSettings(h2: Http2Config): Chunk[(Int, Int)] =
    Chunk(
      3 -> h2.maxConcurrentStreams,
      4 -> h2.initialWindowSize.toInt,
      5 -> h2.maxFrameSize.toInt,
      6 -> h2.maxHeaderListSize.toInt,
      8 -> 1, // SETTINGS_ENABLE_CONNECT_PROTOCOL (RFC 8441)
    )

  private def writer(out: Queue[H2Frame], send: Chunk[Byte] => Task[Unit]): IO[HttpError, Unit] =
    out.take.flatMap(f => write(send, f)).forever

  private def write(send: Chunk[Byte] => Task[Unit], frame: H2Frame): IO[HttpError, Unit] =
    send(FrameCodec.encode(frame)).mapError(HttpError.Io(_))

  private def reader[R](
      routes: Routes[R, Response],
      src: ConnBuf,
      out: Queue[H2Frame],
      flow: H2Flow,
      streams: java.util.concurrent.atomic.AtomicInteger,
      fibers: java.util.Set[Fiber[Any, Any]],
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    val acc                           = scala.collection.concurrent.TrieMap.empty[Int, Chunk[Byte]]
    val bodies                        = scala.collection.concurrent.TrieMap.empty[Int, Queue[Option[Chunk[Byte]]]]
    val received                      = scala.collection.concurrent.TrieMap.empty[Int, Long]
    def loop: ZIO[R, HttpError, Unit] =
      if !takingWork.get() then ZIO.unit
      else
        src.setReadTimeout(config.idleTimeout) *>
          Server
            .awaitWithin(config.idleTimeout)(readFrame(src, config.http2Config.maxFrameSize.toInt))
            .flatMap {
              case None        => ZIO.unit
              case Some(frame) =>
                frame match
                  case None    => ZIO.unit
                  case Some(f) =>
                    handle(
                      routes,
                      out,
                      flow,
                      streams,
                      fibers,
                      config,
                      busy,
                      acc,
                      bodies,
                      received,
                      f,
                      secure,
                    ) *> loop
            }
    loop
  end reader

  private def handle[R](
      routes: Routes[R, Response],
      out: Queue[H2Frame],
      flow: H2Flow,
      streams: java.util.concurrent.atomic.AtomicInteger,
      fibers: java.util.Set[Fiber[Any, Any]],
      config: Server.Config,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      acc: scala.collection.concurrent.TrieMap[Int, Chunk[Byte]],
      bodies: scala.collection.concurrent.TrieMap[Int, Queue[Option[Chunk[Byte]]]],
      received: scala.collection.concurrent.TrieMap[Int, Long],
      frame: H2Frame,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    frame match
      case H2Frame.Settings(true, _)   => ZIO.unit
      case H2Frame.Settings(false, _)  => out.offer(H2Frame.Settings(ack = true, Chunk.empty)).unit
      case H2Frame.Ping(false, opaque) => out.offer(H2Frame.Ping(ack = true, opaque)).unit
      case H2Frame.Ping(true, _)       => ZIO.unit
      case H2Frame.WindowUpdate(id, n) => flow.creditSend(id, n)
      case H2Frame.GoAway(_, _, _)     => ZIO.unit
      case H2Frame.RstStream(id, _)    =>
        received.remove(id)
        flow.close(id) *>
          bodies.remove(id).fold(ZIO.unit)(_.offer(None).unit)
      case H2Frame.Data(id, data, end, _) =>
        val n = data.length
        flow.takeRecv(id, n).flatMap { ok =>
          if !ok then
            received.remove(id)
            bodies.remove(id).fold(ZIO.unit)(_.offer(None).unit) *>
              out.offer(H2Frame.RstStream(id, H2Flow.FlowControlError)).unit
          else
            val total = received.getOrElse(id, 0L) + n
            if total > config.maxBodyBytes.toLong then
              received.remove(id)
              bodies.remove(id).fold(ZIO.unit)(_.offer(None).unit) *>
                out.offer(H2Frame.RstStream(id, H2Flow.RefusedStream)).unit
            else
              received.update(id, total)
              bodies.get(id) match
                case None    => ZIO.unit
                case Some(q) =>
                  val put = if n > 0 then q.offer(Some(data)).unit else ZIO.unit
                  put *>
                    flow.restoreRecv(id, n) *>
                    (if n > 0 then
                       out.offer(H2Frame.WindowUpdate(0, n)).unit *>
                         out.offer(H2Frame.WindowUpdate(id, n)).unit
                     else ZIO.unit) *>
                    (if end then
                       received.remove(id); q.offer(None).unit
                     else ZIO.unit)
              end match
            end if
        }
      case H2Frame.Headers(id, block, endStream, endHeaders, _, _, _) =>
        val prev = acc.getOrElse(id, Chunk.empty)
        val next = prev ++ block
        if endHeaders then
          acc.remove(id)
          openStream(routes, out, flow, streams, fibers, config, busy, bodies, id, next, endStream, secure)
        else
          val _ = acc.put(id, next)
          ZIO.unit
      case H2Frame.Continuation(id, block, endHeaders) =>
        val prev = acc.getOrElse(id, Chunk.empty)
        val next = prev ++ block
        if endHeaders then
          acc.remove(id)
          openStream(routes, out, flow, streams, fibers, config, busy, bodies, id, next, endStream = false, secure)
        else
          acc.put(id, next)
          ZIO.unit
      case _ => ZIO.unit

  private def openStream[R](
      routes: Routes[R, Response],
      out: Queue[H2Frame],
      flow: H2Flow,
      streams: java.util.concurrent.atomic.AtomicInteger,
      fibers: java.util.Set[Fiber[Any, Any]],
      config: Server.Config,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      bodies: scala.collection.concurrent.TrieMap[Int, Queue[Option[Chunk[Byte]]]],
      id: Int,
      block: Chunk[Byte],
      endStream: Boolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    if streams.incrementAndGet() > config.http2Config.maxConcurrentStreams then
      streams.decrementAndGet()
      out.offer(H2Frame.RstStream(id, H2Flow.RefusedStream)).unit
    else
      val hdrs = Hpack.decode(block)
      for
        _ <- flow.open(id)
        q <- Queue.bounded[Option[Chunk[Byte]]](config.http2Config.maxOutstandingFrames)
        _ <- ZIO.succeed { if !endStream then bodies.put(id, q); () }
        body =
          if endStream then Body.empty
          else
            Body.stream(
              ZStream.fromQueue(q).takeWhile(_.isDefined).collect { case Some(c) => c }.flattenChunks,
              None,
              None,
            )
        req = requestOf(hdrs, body, secure)
        _ <- ZIO.succeed(busy.set(true))
        run =
          routes(req)
            .catchAllCause { c =>
              if c.isInterruptedOnly then ZIO.interrupt
              else ZIO.succeed(Response.internalServerError())
            }
            .flatMap(res => respond(out, flow, config, id, res, bodies))
            .ensuring(
              ZIO.succeed {
                busy.set(false)
                streams.decrementAndGet()
                ()
              } *> flow.close(id)
            )
        fiber <- run.forkDaemon
        _     <- ZIO.succeed { fibers.add(fiber); () }
      yield ()
      end for
  end openStream

  private def requestOf(hdrs: Chunk[(String, String)], body: Body, secure: Boolean): Request =
    val map    = hdrs.toMap
    val h2ws   = map.get(":protocol").exists(_.equalsIgnoreCase("websocket"))
    val method =
      if h2ws then Method.GET
      else Method.parse(map.getOrElse(":method", "GET")).getOrElse(Method.GET)
    val path = map.getOrElse(":path", "/")
    var hs   = Headers(hdrs.filterNot(_._1.startsWith(":")).map((n, v) => Header(n, v)))
    if h2ws then hs = hs.add(":protocol", "websocket")
    Request(method, Url.parse(path), hs, body, HttpVersion.Http2, secure)
  end requestOf

  private def respond(
      out: Queue[H2Frame],
      flow: H2Flow,
      config: Server.Config,
      id: Int,
      res: Response,
      bodies: scala.collection.concurrent.TrieMap[Int, Queue[Option[Chunk[Byte]]]],
  ): IO[HttpError, Unit] =
    val hs =
      Chunk(":status" -> res.status.code.toString) ++
        res.headers.toChunk.map(h => h.name.http2 -> h.value)
    val block = Hpack.encode(hs)
    res.ws match
      case Some(run) =>
        val buf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, 4096))
        for
          q <- bodies.get(id) match
            case Some(existing) => ZIO.succeed(existing)
            case None           =>
              Queue.bounded[Option[Chunk[Byte]]](config.http2Config.maxOutstandingFrames).flatMap { nq =>
                ZIO.succeed { bodies.put(id, nq); nq }
              }
          src = ConnBuf.fromPull(
            buf,
            q.take.map {
              case None    => None
              case Some(c) => Some(c)
            },
          )
          send = (c: Chunk[Byte]) => emitData(out, flow, id, c, config.http2Config.maxFrameSize.toInt)
          _ <- out.offer(H2Frame.Headers(id, block, endStream = false, endHeaders = true))
          _ <- run(src, send).mapError(HttpError.Io(_))
          _ <- out.offer(H2Frame.Data(id, Chunk.empty, endStream = true))
        yield ()
        end for
      case None =>
        res.body match
          case Body.Empty =>
            out.offer(H2Frame.Headers(id, block, endStream = true, endHeaders = true)).unit
          case Body.Bytes(bytes, _) =>
            out.offer(H2Frame.Headers(id, block, endStream = false, endHeaders = true)) *>
              dataFrames(out, flow, config, id, ZStream.fromChunk(bytes))
          case Body.Stream(s, _, _) =>
            out.offer(H2Frame.Headers(id, block, endStream = false, endHeaders = true)) *>
              dataFrames(out, flow, config, id, s)
    end match
  end respond

  private def dataFrames(
      out: Queue[H2Frame],
      flow: H2Flow,
      config: Server.Config,
      id: Int,
      stream: ZStream[Any, Throwable, Byte],
  ): IO[HttpError, Unit] =
    val max = config.http2Config.maxFrameSize.toInt
    stream
      .mapChunksZIO { c =>
        if c.isEmpty then ZIO.succeed(c)
        else emitData(out, flow, id, c, max).as(c)
      }
      .runDrain
      .mapError(HttpError.Io(_)) *>
      out.offer(H2Frame.Data(id, Chunk.empty, endStream = true)).unit
  end dataFrames

  private def emitData(
      out: Queue[H2Frame],
      flow: H2Flow,
      id: Int,
      data: Chunk[Byte],
      max: Int,
  ): UIO[Unit] =
    if data.length <= max then
      flow.takeSend(id, data.length) *> out.offer(H2Frame.Data(id, data, endStream = false)).unit
    else
      val head = data.take(max)
      flow.takeSend(id, head.length) *>
        out.offer(H2Frame.Data(id, head, endStream = false)).unit *>
        emitData(out, flow, id, data.drop(max), max)

  private def readFrame(src: ConnBuf, max: Int): IO[HttpError, Option[H2Frame]] =
    src.takeExact(9).flatMap { hdr =>
      if hdr.isEmpty then ZIO.succeed(None)
      else if hdr.length < 9 then ZIO.fail(HttpError.Malformed("truncated h2 header"))
      else
        val len = ((hdr(0) & 0xff) << 16) | ((hdr(1) & 0xff) << 8) | (hdr(2) & 0xff)
        if len > max then ZIO.fail(HttpError.Malformed("h2 frame too large"))
        else
          src.takeExact(len).flatMap { payload =>
            if payload.length < len then ZIO.fail(HttpError.Malformed("truncated h2 frame"))
            else
              FrameCodec.decode(hdr ++ payload, max) match
                case Right((f, _)) => ZIO.succeed(Some(f))
                case Left(err)     => ZIO.fail(HttpError.Malformed(err))
          }
    }
end H2Connection
