package heddle.internal.h2

import heddle.error.{H2Violation, HttpError, WireError}
import heddle.http.{Body, HttpVersion, Method, Request, Response, Url}
import heddle.http.header.{Header, Headers}
import heddle.internal.engine.ConnBuf
import heddle.route.Routes
import heddle.Server
import heddle.server.Http2Config
import zio.*
import zio.stream.ZStream

/** One HTTP/2 server connection (RFC 9113). A single reader fiber owns the frame state; each stream's handler runs in
  * its own fiber, owned by the connection and interrupted with it; one writer sends whole batches, so a HEADERS frame
  * and its CONTINUATIONs are never split by another stream's frames.
  */
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
    ZIO.scoped[R] {
      for
        conn    <- Conn.make(config, busy, secure)
        drained <- Promise.make[Nothing, Unit]
        _       <- write(send, Chunk.single(H2Frame.Settings(localSettings(h2))))
        writer  <- conn.out.take
          .flatMap(batch => write(send, batch) *> drained.succeed(()).when(batch.exists(isGoAway)))
          .forever
          .forkScoped
        // The GOAWAY is the last batch; wait for the writer to send it, or to have died trying.
        last = (code: Int) => goAway(conn, code) *> (drained.await race writer.await.unit)
        _ <- reader(routes, src, conn, takingWork)
          .foldZIO(
            {
              case HttpError.Malformed(e) => last(e.h2Code) *> ZIO.fail(HttpError.Malformed(e))
              case other                  => ZIO.fail(other)
            },
            _ => last(0x0),
          )
          .ensuring(conn.fibers.get.flatMap(fs => ZIO.foreachParDiscard(fs.values)(_.interrupt)))
      yield ()
    }
  end serve

  /** What every stream fiber shares. `fibers` holds the running handlers, each removed when it ends. */
  private final class Conn(
      val out: Queue[Chunk[H2Frame]],
      val flow: H2Flow,
      val config: Server.Config,
      val busy: java.util.concurrent.atomic.AtomicBoolean,
      val secure: Boolean,
      val peerMaxFrame: Ref[Int],
      val lastStream: Ref[Int],
      val fibers: Ref[Map[Int, Fiber[Nothing, Unit]]],
  ):
    val h2: Http2Config = config.http2Config

    def offer(frames: H2Frame*): UIO[Unit] = out.offer(Chunk.fromIterable(frames)).unit
  end Conn

  private object Conn:
    def make(config: Server.Config, busy: java.util.concurrent.atomic.AtomicBoolean, secure: Boolean): UIO[Conn] =
      for
        out    <- Queue.bounded[Chunk[H2Frame]](config.http2Config.maxOutstandingFrames)
        flow   <- H2Flow.make(config.http2Config.initialWindowSize.toInt)
        frame  <- Ref.make(H2Frame.DefaultMaxFrameSize)
        last   <- Ref.make(0)
        fibers <- Ref.make(Map.empty[Int, Fiber[Nothing, Unit]])
      yield Conn(out, flow, config, busy, secure, frame, last, fibers)

  /** The reader's own state: an unfinished header block, the peer's HPACK table, and each open request body. */
  private final case class ReaderState(
      pending: Option[Pending],
      hpack: HpackTable,
      bodies: Map[Int, Inbound],
  )

  private final case class Pending(stream: Int, block: Chunk[Byte], endStream: Boolean)

  private final case class Inbound(queue: Queue[Option[Chunk[Byte]]], received: Long)

  private def localSettings(h2: Http2Config): Chunk[(Int, Int)] =
    Chunk(
      3 -> h2.maxConcurrentStreams,
      4 -> h2.initialWindowSize.toInt,
      5 -> h2.maxFrameSize.toInt,
      6 -> h2.maxHeaderListSize.toInt,
      8 -> 1, // SETTINGS_ENABLE_CONNECT_PROTOCOL (RFC 8441)
    )

  private def isGoAway(f: H2Frame): Boolean =
    f match
      case _: H2Frame.GoAway => true
      case _                 => false

  private def goAway(conn: Conn, code: Int): UIO[Unit] =
    conn.lastStream.get.flatMap(last => conn.offer(H2Frame.GoAway(last, code)))

  private def write(send: Chunk[Byte] => Task[Unit], frames: Chunk[H2Frame]): IO[HttpError, Unit] =
    send(frames.flatMap(FrameCodec.encode)).mapError(HttpError.Io(_))

  private def violation(v: H2Violation): IO[HttpError, Nothing] =
    ZIO.fail(HttpError.Malformed(WireError.H2Protocol(v)))

  private def reader[R](
      routes: Routes[R, Response],
      src: ConnBuf,
      conn: Conn,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
  ): ZIO[R, HttpError, Unit] =
    def loop(state: ReaderState): ZIO[R, HttpError, Unit] =
      if !takingWork.get() then ZIO.unit
      else
        src.setReadTimeout(conn.config.idleTimeout) *>
          Server
            .awaitWithin(conn.config.idleTimeout)(readFrame(src, conn.h2.maxFrameSize.toInt))
            .flatMap {
              case Some(Some(frame)) => handle(routes, conn, state, frame).flatMap(loop)
              case _                 => ZIO.unit
            }
    loop(ReaderState(None, HpackTable.empty, Map.empty))
  end reader

  private def handle[R](
      routes: Routes[R, Response],
      conn: Conn,
      state: ReaderState,
      frame: H2Frame,
  ): ZIO[R, HttpError, ReaderState] =
    (state.pending, frame) match
      case (Some(p), H2Frame.Continuation(id, block, endHeaders)) if id == p.stream =>
        val next = p.copy(block = p.block ++ block)
        if endHeaders then headerBlock(routes, conn, state.copy(pending = None), next)
        else ZIO.succeed(state.copy(pending = Some(next)))
      case (Some(p), other) =>
        violation(H2Violation.ContinuationExpected(p.stream, frameType(other)))
      case (None, H2Frame.Continuation(id, _, _)) =>
        violation(H2Violation.UnexpectedContinuation(id))
      case (None, H2Frame.Headers(id, block, endStream, endHeaders, _, _, _)) =>
        val p = Pending(id, block, endStream)
        if endHeaders then headerBlock(routes, conn, state, p) else ZIO.succeed(state.copy(pending = Some(p)))
      case (None, H2Frame.Data(id, data, end, _)) =>
        inbound(conn, state, id, data, end)
      case (None, H2Frame.Settings(params)) =>
        ZIO.foreachDiscard(params)(setting(conn, _)) *> conn.offer(H2Frame.SettingsAck).as(state)
      case (None, H2Frame.Ping(opaque)) =>
        conn.offer(H2Frame.PingAck(opaque)).as(state)
      case (None, H2Frame.WindowUpdate(0, 0)) =>
        violation(H2Violation.ZeroIncrement)
      case (None, H2Frame.WindowUpdate(id, 0)) =>
        conn.offer(H2Frame.RstStream(id, 0x1)).as(state)
      case (None, H2Frame.WindowUpdate(id, n)) =>
        conn.flow.creditSend(id, n).flatMap { ok =>
          if ok then ZIO.succeed(state)
          else if id == 0 then violation(H2Violation.WindowOverflow(0))
          else conn.offer(H2Frame.RstStream(id, H2Flow.FlowControlError)).as(state)
        }
      case (None, H2Frame.RstStream(id, _)) =>
        conn.fibers.get.flatMap(fs => ZIO.foreachDiscard(fs.get(id))(_.interruptFork)) *> close(conn, state, id)
      case (None, _) =>
        ZIO.succeed(state)
    end match
  end handle

  private def frameType(f: H2Frame): Int =
    f match
      case _: H2Frame.Data                           => H2Frame.DataType
      case _: H2Frame.Headers                        => H2Frame.HeadersType
      case _: H2Frame.Priority                       => H2Frame.PriorityType
      case _: H2Frame.RstStream                      => H2Frame.RstStreamType
      case _: H2Frame.Settings | H2Frame.SettingsAck => H2Frame.SettingsType
      case _: H2Frame.PushPromise                    => H2Frame.PushPromiseType
      case _: H2Frame.Ping | _: H2Frame.PingAck      => H2Frame.PingType
      case _: H2Frame.GoAway                         => H2Frame.GoAwayType
      case _: H2Frame.WindowUpdate                   => H2Frame.WindowUpdateType
      case _: H2Frame.Continuation                   => H2Frame.ContinuationType
      case u: H2Frame.Unknown                        => u.frameType

  /** RFC 9113 §6.5.2. The settings this server sends with no pushes or table of its own need no answer beyond the ACK.
    */
  private def setting(conn: Conn, param: (Int, Int)): IO[HttpError, Unit] =
    val (id, raw) = param
    val value     = raw.toLong & 0xffffffffL
    id match
      case 2 if value > 1                => violation(H2Violation.BadSetting(id, value))
      case 4 if value > H2Flow.MaxWindow => violation(H2Violation.BadSetting(id, value))
      case 4                             => conn.flow.resizeSend(value)
      case 5 if value < H2Frame.MinMaxFrameSize || value > H2Frame.MaxMaxFrameSize =>
        violation(H2Violation.BadSetting(id, value))
      case 5 => conn.peerMaxFrame.set(value.toInt)
      case _ => ZIO.unit
  end setting

  /** A whole header block: a new request, or the trailers that end an open one. Either way it updates the HPACK table.
    */
  private def headerBlock[R](
      routes: Routes[R, Response],
      conn: Conn,
      state: ReaderState,
      p: Pending,
  ): ZIO[R, HttpError, ReaderState] =
    ZIO
      .fromEither(Hpack.decode(p.block, state.hpack, conn.h2.maxHeaderListSize.toLong))
      .mapError(e => HttpError.Malformed(WireError.BadHeaderBlock(e)))
      .flatMap { (fields, hpack) =>
        val next = state.copy(hpack = hpack)
        next.bodies.get(p.stream) match
          case Some(body) => body.queue.offer(None).as(next.copy(bodies = next.bodies - p.stream))
          case None       => open(routes, conn, next, p.stream, fields, p.endStream)
      }

  private def open[R](
      routes: Routes[R, Response],
      conn: Conn,
      state: ReaderState,
      id: Int,
      fields: Chunk[(String, String)],
      endStream: Boolean,
  ): ZIO[R, HttpError, ReaderState] =
    conn.lastStream.get.flatMap { last =>
      if id % 2 == 0 then violation(H2Violation.EvenStreamId(id))
      else if id <= last then violation(H2Violation.StreamIdRegressed(id, last))
      else
        conn.lastStream.set(id) *> conn.fibers.get.flatMap { running =>
          if running.size >= conn.h2.maxConcurrentStreams then
            conn.offer(H2Frame.RstStream(id, H2Flow.RefusedStream)).as(state)
          else
            Queue.bounded[Option[Chunk[Byte]]](conn.h2.maxOutstandingFrames).flatMap { q =>
              requestOf(fields, body(q, endStream), conn.secure) match
                case None =>
                  conn.offer(H2Frame.RstStream(id, 0x1)).as(state)
                case Some(req) =>
                  start(routes, conn, id, req, q).as(
                    if endStream then state else state.copy(bodies = state.bodies + (id -> Inbound(q, 0L)))
                  )
            }
        }
    }

  private def body(q: Queue[Option[Chunk[Byte]]], endStream: Boolean): Body =
    if endStream then Body.empty
    else Body.stream(ZStream.fromQueue(q).collectWhileSome.flattenChunks, None, None)

  /** Forks the handler, owned by the connection: it is in `fibers` before it runs and removed when it ends. */
  private def start[R](
      routes: Routes[R, Response],
      conn: Conn,
      id: Int,
      req: Request,
      q: Queue[Option[Chunk[Byte]]],
  ): URIO[R, Unit] =
    val run =
      routes(req)
        .catchAllCause(c => if c.isInterruptedOnly then ZIO.interrupt else ZIO.succeed(Response.internalServerError()))
        .flatMap(res => respond(conn, id, res, q))
        .catchAll(_ => conn.offer(H2Frame.RstStream(id, 0x2)))
    for
      gate  <- Promise.make[Nothing, Unit]
      _     <- ZIO.succeed(conn.busy.set(true))
      fiber <- (gate.await *> run)
        .ensuring(
          conn.flow
            .close(id) *> conn.fibers.updateAndGet(_ - id).flatMap(left => ZIO.succeed(conn.busy.set(left.nonEmpty)))
        )
        .forkDaemon
      _ <- conn.fibers.update(_ + (id -> fiber)) *> conn.flow.open(id) *> gate.succeed(())
    yield ()
    end for
  end start

  private def inbound(
      conn: Conn,
      state: ReaderState,
      id: Int,
      data: Chunk[Byte],
      end: Boolean,
  ): UIO[ReaderState] =
    val n = data.length
    conn.flow.takeRecv(id, n).flatMap { ok =>
      state.bodies.get(id) match
        // A closed stream's DATA still counts against the connection window (RFC 9113 §6.9), so it is credited back.
        case None           => conn.offer(H2Frame.WindowUpdate(0, n)).when(n > 0).as(state)
        case Some(_) if !ok =>
          conn.offer(H2Frame.RstStream(id, H2Flow.FlowControlError)) *> close(conn, state, id)
        case Some(in) if in.received + n > conn.config.maxBodyBytes.toLong =>
          conn.offer(H2Frame.RstStream(id, H2Flow.RefusedStream)) *> close(conn, state, id)
        case Some(in) =>
          val credit =
            if n > 0 then
              conn.flow.restoreRecv(id, n) *> conn.offer(H2Frame.WindowUpdate(0, n), H2Frame.WindowUpdate(id, n))
            else ZIO.unit
          val put = if n > 0 then in.queue.offer(Some(data)).unit else ZIO.unit
          put *> credit *>
            (if end then in.queue.offer(None).as(state.copy(bodies = state.bodies - id))
             else ZIO.succeed(state.copy(bodies = state.bodies.updated(id, in.copy(received = in.received + n)))))
    }
  end inbound

  private def close(conn: Conn, state: ReaderState, id: Int): UIO[ReaderState] =
    conn.flow.close(id) *>
      ZIO.foreachDiscard(state.bodies.get(id))(_.queue.offer(None)).as(state.copy(bodies = state.bodies - id))

  /** RFC 9113 §8.3.1: `:method`, `:scheme`, and `:path` are required, except that extended CONNECT (RFC 8441) names a
    * `:protocol`. A request without them is malformed.
    */
  private def requestOf(fields: Chunk[(String, String)], body: Body, secure: Boolean): Option[Request] =
    val pseudo   = fields.filter(_._1.startsWith(":")).toMap
    val headers  = Headers(fields.filterNot(_._1.startsWith(":")).map((n, v) => Header(n, v)))
    val protocol = pseudo.get(":protocol")
    for
      method <- pseudo.get(":method").flatMap(Method.parse)
      path   <- pseudo.get(":path").filter(_.nonEmpty)
      _      <- pseudo.get(":scheme").orElse(protocol)
    yield
      val ws = method == Method.CONNECT && protocol.exists(_.equalsIgnoreCase("websocket"))
      Request(
        if ws then Method.GET else method,
        Url.parse(path),
        if ws then headers.add(":protocol", "websocket") else headers,
        body,
        HttpVersion.Http2,
        secure,
      )
    end for
  end requestOf

  private def respond(conn: Conn, id: Int, res: Response, q: Queue[Option[Chunk[Byte]]]): IO[HttpError, Unit] =
    val block = Hpack.encode(
      Chunk(":status" -> res.status.code.toString) ++ res.headers.toChunk.map(h => h.name.http2 -> h.value)
    )
    res.ws match
      case Some(run) =>
        val buf  = java.nio.ByteBuffer.allocate(math.max(conn.config.chunkSize.toInt, 4096))
        val src  = ConnBuf.fromPull(buf, q.take)
        val send = (c: Chunk[Byte]) => emitData(conn, id, c).unit
        headers(conn, id, block, endStream = false) *>
          run(src, send).mapError(HttpError.Io(_)) *>
          conn.offer(H2Frame.Data(id, Chunk.empty, endStream = true))
      case None =>
        res.body match
          case Body.Empty           => headers(conn, id, block, endStream = true)
          case Body.Bytes(bytes, _) =>
            headers(conn, id, block, endStream = false) *> dataFrames(conn, id, ZStream.fromChunk(bytes))
          case Body.Stream(s, _, _) => headers(conn, id, block, endStream = false) *> dataFrames(conn, id, s)
    end match
  end respond

  /** One batch: HEADERS, then CONTINUATIONs cut to the peer's SETTINGS_MAX_FRAME_SIZE (RFC 9113 §6.10). */
  private def headers(conn: Conn, id: Int, block: Chunk[Byte], endStream: Boolean): UIO[Unit] =
    conn.peerMaxFrame.get.flatMap { max =>
      val pieces = if block.isEmpty then Chunk.single(block) else Chunk.fromIterator(block.grouped(max))
      val last   = pieces.length - 1
      val frames = pieces.zipWithIndex.map { (piece, i) =>
        if i == 0 then H2Frame.Headers(id, piece, endStream, endHeaders = i == last)
        else H2Frame.Continuation(id, piece, endHeaders = i == last)
      }
      conn.out.offer(frames).unit
    }

  private def dataFrames(conn: Conn, id: Int, stream: ZStream[Any, Throwable, Byte]): IO[HttpError, Unit] =
    stream.chunks
      .runFoldWhileZIO(true)(identity)((_, c) => if c.isEmpty then ZIO.succeed(true) else emitData(conn, id, c))
      .mapError(HttpError.Io(_))
      .flatMap(open => conn.offer(H2Frame.Data(id, Chunk.empty, endStream = true)).when(open).unit)

  /** Sends `data` as the send windows open and the peer's frame size allows; `false` once the stream is gone. */
  private def emitData(conn: Conn, id: Int, data: Chunk[Byte]): UIO[Boolean] =
    if data.isEmpty then ZIO.succeed(true)
    else
      conn.peerMaxFrame.get.flatMap { max =>
        conn.flow.takeSend(id, math.min(max, data.length)).flatMap {
          case None    => ZIO.succeed(false)
          case Some(n) =>
            conn.offer(H2Frame.Data(id, data.take(n), endStream = false)) *> emitData(conn, id, data.drop(n))
        }
      }

  private def readFrame(src: ConnBuf, max: Int): IO[HttpError, Option[H2Frame]] =
    src.takeExact(9).flatMap { hdr =>
      if hdr.isEmpty then ZIO.succeed(None)
      else if hdr.length < 9 then ZIO.fail(HttpError.Malformed(WireError.TruncatedFrame))
      else
        val len = ((hdr(0) & 0xff) << 16) | ((hdr(1) & 0xff) << 8) | (hdr(2) & 0xff)
        if len > max then ZIO.fail(HttpError.Malformed(WireError.FrameTooLarge(len, max)))
        else
          src.takeExact(len).flatMap { payload =>
            if payload.length < len then ZIO.fail(HttpError.Malformed(WireError.TruncatedFrame))
            else ZIO.fromEither(FrameCodec.decode(hdr ++ payload, max)).mapBoth(HttpError.Malformed(_), f => Some(f._1))
          }
    }
end H2Connection
