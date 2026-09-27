package heddle

import heddle.error.{HttpError, ServerError, WireError}
import heddle.http.Response
import heddle.internal.duplex.{ByteConn, ChannelListener}
import heddle.internal.engine.{ConnBuf, Http1, LiveConnections}
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
      _        <- enableLoom
      clock    <- ZIO.clock
      live     <- LiveConnections.make(config.maxConnections)
      listener <- ZIO.acquireRelease(ChannelListener.bind(config))(_.close)
      halt0 = live.halt(listener, config.gracefulShutdownTimeout).withClock(clock)
      _ <- live.acceptAll(listener)(runConnection(routes, _, config, live.takingWork, _, tls)).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(listener.localPort, halt0)
    end for
  end install

  /** Connection fibers run on virtual threads; a JVM that cannot enable them still binds on the default executor. */
  private val enableLoom: URIO[Scope, Unit] =
    (Runtime.enableLoomBasedExecutor ++ Runtime.enableLoomBasedBlockingExecutor).build.unit.ignore

  private def runConnection[R](
      routes: Routes[R, Response],
      conn: ByteConn,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, HttpError, Unit] =
    val readBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    tls match
      case None =>
        val src  = ConnBuf.fromConn(readBuf, conn)
        val send = conn.write
        serve(routes, src, send, config, takingWork, busy, secure = false)
      case Some(t) =>
        val alpn = if config.http2 then Chunk("h2", "http/1.1") else Chunk("http/1.1")
        t.server(conn, alpn).flatMap { session =>
          val src   = ConnBuf.fromConn(readBuf, session.conn)
          val send  = session.conn.write
          val proto = session.applicationProtocol
          val work  =
            if proto == "h2" && config.http2 then
              consumePreface(src) *>
                heddle.internal.h2.H2Connection.serve(routes, src, send, config, takingWork, busy, secure = true)
            else Http1.serveConnection(routes, src, send, config, takingWork, busy, secure = true)
          work.ensuring(session.conn.close)
        }
    end match
  end runConnection

  private def serve[R](
      routes: Routes[R, Response],
      src: ConnBuf,
      send: Chunk[Byte] => Task[Unit],
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    if !config.http2 then Http1.serveConnection(routes, src, send, config, takingWork, busy, secure)
    else
      val preface = heddle.internal.h2.H2Frame.Preface
      src.setReadTimeout(config.headerTimeout) *>
        Server.awaitWithin(config.headerTimeout)(src.fillUntil(preface.length)).flatMap {
          case None        => ZIO.fail(HttpError.Timeout)
          case Some(avail) =>
            if avail >= preface.length && src.hasPrefix(preface) then
              src.takeExact(preface.length) *>
                heddle.internal.h2.H2Connection.serve(routes, src, send, config, takingWork, busy, secure)
            else Http1.serveConnection(routes, src, send, config, takingWork, busy, secure)
        }

  private def consumePreface(src: ConnBuf): IO[HttpError, Unit] =
    val preface = heddle.internal.h2.H2Frame.Preface
    src.fillUntil(preface.length).flatMap { avail =>
      if avail >= preface.length && src.hasPrefix(preface) then src.takeExact(preface.length).unit
      else ZIO.fail(HttpError.Malformed(WireError.NoH2Preface))
    }
end ServerPlatform
