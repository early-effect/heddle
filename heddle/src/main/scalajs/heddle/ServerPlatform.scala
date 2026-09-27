package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.internal.duplex.{ByteConn, Listener, NodeListener}
import heddle.internal.engine.{ConnBuf, Http1, LiveConnections}
import heddle.route.Routes
import heddle.server.Tls
import zio.*

private[heddle] object ServerPlatform:
  def sbtInterruptExit: UIO[Unit] = ZIO.unit

  def install[R](routes: Routes[R, Response], config: Server.Config): ZIO[R & Scope, ServerError, Server] =
    install(routes, config, JvmScheduler.Loom)

  def install[R](
      routes: Routes[R, Response],
      config: Server.Config,
      scheduler: JvmScheduler,
  ): ZIO[R & Scope, ServerError, Server] =
    val _ = scheduler
    for
      clock    <- ZIO.clock
      tls      <- ZIO.environmentWith[R & Scope](_.getDynamic[Tls])
      live     <- LiveConnections.make(config.maxConnections)
      listener <- ZIO.acquireRelease(bind(config, tls))(_.close)
      halt0 = live.halt(listener, config.gracefulShutdownTimeout).withClock(clock)
      _ <- live.acceptAll(listener)(runConnection(routes, _, config, live.takingWork, _, tls.isDefined)).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(listener.localPort, halt0)
    end for
  end install

  private def bind(config: Server.Config, tls: Option[Tls]): IO[ServerError, Listener] =
    tls match
      case None    => NodeListener.plain(config)
      case Some(t) => t.listener(config)

  private def runConnection[R](
      routes: Routes[R, Response],
      conn: ByteConn,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    val readBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    val src     = ConnBuf.fromConn(readBuf, conn)
    val send    = conn.write
    Http1.serveConnection(routes, src, send, config, takingWork, busy, secure)
  end runConnection
end ServerPlatform
