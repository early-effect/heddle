package heddle

import heddle.error.{HttpError, ServerError, WireError}
import heddle.http.Response
import heddle.http.header.OriginDate
import heddle.internal.duplex.{ChannelConn, ChannelListener}
import heddle.internal.engine.{ConnBuf, Http1, Lifecycle, LiveConnections, PrefaceLook, Wire}
import heddle.route.Routes
import heddle.server.Tls
import zio.*

private[heddle] object ServerPlatform:
  /** sbt 2 client-side run treats JVM SIGINT status 130 as a crash. Consume INT and exit 0; ZIOApp shutdown still
    * interrupts [[Server.serve]].
    */
  def sbtInterruptExit: UIO[Unit] =
    ZIO.succeed {
      sun.misc.Signal.handle(sun.misc.Signal("INT"), _ => java.lang.System.exit(0))
      ()
    }

  def install[R](
      routes: Routes[R, Response],
      config: Server.Config,
      tls: Option[Tls],
  ): ZIO[R & Scope, ServerError, Server] =
    for
      _          <- enableLoom
      clock      <- ZIO.clock
      live       <- LiveConnections.make(config.maxConnections.toInt)
      originDate <- OriginDate.make
      listener   <- ZIO.acquireRelease(ChannelListener.bind(config))(_.close)
      // Live clock plus SO_TIMEOUT: the read stays on the connection's virtual thread. TestClock keeps the ZIO race,
      // which can interrupt a blocking read. A wrapped clock is not `eq` ClockLive, so it keeps the race too.
      socketDeadline = clock eq Clock.ClockLive
      halt0          = live.halt(listener, config.gracefulShutdownTimeout).withClock(clock)
      _ <- live
        .acceptAll(listener)(runConnection(routes, _, config, live.takingWork, _, tls, socketDeadline, originDate))
        .forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(listener.localPort, halt0)
    end for
  end install

  /** Connection fibers run on virtual threads; a JVM that cannot enable them still binds on the default executor. */
  private val enableLoom: URIO[Scope, Unit] =
    (Runtime.enableLoomBasedExecutor ++ Runtime.enableLoomBasedBlockingExecutor).build.unit.ignore

  private def runConnection[R](
      routes: Routes[R, Response],
      conn: ChannelConn,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
      socketDeadline: Boolean,
      originDate: OriginDate,
  ): ZIO[R, HttpError, Unit] =
    val readBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    val life    = Lifecycle(takingWork, busy)
    tls match
      case None =>
        serve(
          routes,
          Wire(ConnBuf.fromConn(readBuf, conn, socketDeadline), conn.write, secure = false),
          config,
          life,
          originDate,
        )
      case Some(t) =>
        val alpn = if config.http2 then Chunk("h2", "http/1.1") else Chunk("http/1.1")
        t.server(conn, alpn).flatMap { session =>
          val wire = Wire(ConnBuf.fromConn(readBuf, session.conn, socketDeadline), session.conn.write, secure = true)
          val work =
            if session.applicationProtocol == "h2" && config.http2 then
              consumePreface(wire.src) *>
                heddle.internal.h2.H2Connection.serve(routes, wire, config, life, originDate)
            else Http1.serveConnection(routes, wire, config, life, originDate)
          work.ensuring(session.conn.close)
        }
    end match
  end runConnection

  /** Plain HTTP: HTTP/2 when the client opens with its preface (prior knowledge), HTTP/1.1 otherwise.
    *
    * A finished HTTP/1 message is dispatched as soon as it is buffered. The preface wait does not demand 24 bytes once
    * the bytes have diverged. A header timeout is handed to HTTP/1 with the budget already spent, so that path writes
    * 408 instead of opening a second window.
    */
  private def serve[R](
      routes: Routes[R, Response],
      wire: Wire,
      config: Server.Config,
      life: Lifecycle,
      originDate: OriginDate,
  ): ZIO[R, HttpError, Unit] =
    if !config.http2 then Http1.serveConnection(routes, wire, config, life, originDate)
    else
      val preface = heddle.internal.h2.H2Frame.Preface
      val src     = wire.src
      val max     = config.maxHeaderBytes.toInt

      def dispatch(look: PrefaceLook): ZIO[R, HttpError, Unit] =
        look match
          case PrefaceLook.Http2 =>
            src.clearHeaderBudget()
            src.takeExact(preface.length) *>
              heddle.internal.h2.H2Connection.serve(routes, wire, config, life, originDate)
          case PrefaceLook.Http1 | PrefaceLook.HeadersTooLarge | PrefaceLook.NeedMore =>
            Http1.serveConnection(routes, wire, config, life, originDate)

      def collect: IO[HttpError, PrefaceLook] =
        ZIO.suspendSucceed {
          src.prefaceLook(preface, max) match
            case PrefaceLook.NeedMore =>
              src.fillPreface(config.headerTimeout).flatMap {
                case false => ZIO.succeed(PrefaceLook.Http1)
                case true  => collect
              }
            case other => ZIO.succeed(other)
        }

      val read =
        if src.socketDeadline then collect
        else
          Server.awaitWithin(config.headerTimeout)(collect).flatMap {
            case Some(look) => ZIO.succeed(look)
            case None       =>
              src.prefaceLook(preface, max) match
                case PrefaceLook.NeedMore =>
                  src.expireHeaderBudget()
                  ZIO.succeed(PrefaceLook.NeedMore)
                case other => ZIO.succeed(other)
          }

      val started =
        if src.socketDeadline then src.setReadTimeout(config.headerTimeout) *> read
        else read

      started.catchSome { case HttpError.Timeout => ZIO.succeed(PrefaceLook.NeedMore) }.flatMap(dispatch)
    end if
  end serve

  private def consumePreface(src: ConnBuf): IO[HttpError, Unit] =
    val preface = heddle.internal.h2.H2Frame.Preface
    src.fillUntil(preface.length).flatMap { avail =>
      if avail >= preface.length && src.hasPrefix(preface) then src.takeExact(preface.length).unit
      else ZIO.fail(HttpError.Malformed(WireError.NoH2Preface))
    }
end ServerPlatform
