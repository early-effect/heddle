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
      out <- Queue.unbounded[H2Frame]
      _   <- write(send, H2Frame.Settings(ack = false, localSettings(h2)))
      wf  <- writer(out, send).forkDaemon
      _   <- (reader(routes, src, out, config, takingWork, busy, secure) *>
        out.offer(H2Frame.GoAway(0, 0x0)).unit)
        .ensuring(out.shutdown *> wf.interrupt)
    yield ()
  end serve

  private def localSettings(h2: Http2Config): Chunk[(Int, Int)] =
    Chunk(
      3 -> h2.maxConcurrentStreams,
      4 -> h2.initialWindowSize,
      5 -> h2.maxFrameSize,
      6 -> h2.maxHeaderListSize,
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
        readFrame(src, config.http2Config.maxFrameSize).flatMap {
          case None        => ZIO.unit
          case Some(frame) => handle(routes, out, config, busy, acc, bodies, received, frame, secure) *> loop
        }
    loop
  end reader

  private def handle[R](
      routes: Routes[R, Response],
      out: Queue[H2Frame],
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
      case H2Frame.WindowUpdate(_, _)  => ZIO.unit
      case H2Frame.GoAway(_, _, _)     => ZIO.unit
      case H2Frame.RstStream(id, _)    =>
        received.remove(id)
        bodies.remove(id).fold(ZIO.unit)(_.offer(None).unit)
      case H2Frame.Data(id, data, end, _) =>
        val n = received.getOrElse(id, 0L) + data.length
        if n > config.maxBodyBytes then
          received.remove(id)
          bodies.remove(id).fold(ZIO.unit)(_.offer(None).unit) *>
            out.offer(H2Frame.RstStream(id, 0x7)).unit
        else
          received.update(id, n)
          bodies.get(id) match
            case None    => ZIO.unit
            case Some(q) =>
              val put = if data.nonEmpty then q.offer(Some(data)).unit else ZIO.unit
              put *> (if end then
                        received.remove(id); q.offer(None).unit
                      else ZIO.unit)
        end if
      case H2Frame.Headers(id, block, endStream, endHeaders, _, _, _) =>
        val prev = acc.getOrElse(id, Chunk.empty)
        val next = prev ++ block
        if endHeaders then
          acc.remove(id)
          openStream(routes, out, config, busy, bodies, id, next, endStream, secure)
        else
          val _ = acc.put(id, next)
          ZIO.unit
      case H2Frame.Continuation(id, block, endHeaders) =>
        val prev = acc.getOrElse(id, Chunk.empty)
        val next = prev ++ block
        if endHeaders then
          acc.remove(id)
          openStream(routes, out, config, busy, bodies, id, next, endStream = false, secure)
        else
          acc.put(id, next)
          ZIO.unit
      case _ => ZIO.unit

  private def openStream[R](
      routes: Routes[R, Response],
      out: Queue[H2Frame],
      config: Server.Config,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      bodies: scala.collection.concurrent.TrieMap[Int, Queue[Option[Chunk[Byte]]]],
      id: Int,
      block: Chunk[Byte],
      endStream: Boolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    val hdrs = Hpack.decode(block)
    for
      q <- Queue.unbounded[Option[Chunk[Byte]]]
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
          .flatMap(res => respond(out, config, id, res, bodies))
          .ensuring(ZIO.succeed(busy.set(false)))
      _ <- run.forkDaemon
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
        val buf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize, 4096))
        for
          q <- bodies.get(id) match
            case Some(existing) => ZIO.succeed(existing)
            case None           =>
              Queue.unbounded[Option[Chunk[Byte]]].flatMap { nq =>
                ZIO.succeed { bodies.put(id, nq); nq }
              }
          src = ConnBuf.fromPull(
            buf,
            q.take.map {
              case None    => None
              case Some(c) => Some(c)
            },
          )
          send = (c: Chunk[Byte]) => emitData(out, id, c, config.http2Config.maxFrameSize)
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
              dataFrames(out, config, id, ZStream.fromChunk(bytes))
          case Body.Stream(s, _, _) =>
            out.offer(H2Frame.Headers(id, block, endStream = false, endHeaders = true)) *>
              dataFrames(out, config, id, s)
    end match
  end respond

  private def dataFrames(
      out: Queue[H2Frame],
      config: Server.Config,
      id: Int,
      stream: ZStream[Any, Throwable, Byte],
  ): IO[HttpError, Unit] =
    val max = config.http2Config.maxFrameSize
    stream
      .mapChunksZIO { c =>
        if c.isEmpty then ZIO.succeed(c)
        else emitData(out, id, c, max).as(c)
      }
      .runDrain
      .mapError(HttpError.Io(_)) *>
      out.offer(H2Frame.Data(id, Chunk.empty, endStream = true)).unit
  end dataFrames

  private def emitData(out: Queue[H2Frame], id: Int, data: Chunk[Byte], max: Int): UIO[Unit] =
    if data.length <= max then out.offer(H2Frame.Data(id, data, endStream = false)).unit
    else
      out.offer(H2Frame.Data(id, data.take(max), endStream = false)).unit *>
        emitData(out, id, data.drop(max), max)

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
