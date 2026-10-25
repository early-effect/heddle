package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.http.header.OriginDate
import heddle.internal.duplex.{Listener, NodeConn, NodeListener}
import heddle.internal.engine.{ConnBuf, Http1, Lifecycle, LiveConnections, Wire}
import heddle.route.Routes
import heddle.server.Tls
import zio.*

private[heddle] object ServerPlatform:
  def sbtInterruptExit: UIO[Unit] = ZIO.unit

  def install[R](
      routes: Routes[R, Response],
      config: Server.Config,
      tls: Option[Tls],
  ): ZIO[R & Scope, ServerError, Server] =
    for
      clock      <- ZIO.clock
      live       <- LiveConnections.make(config.maxConnections.toInt)
      originDate <- OriginDate.make
      listener   <- ZIO.acquireRelease(bind(config, tls))(_.close)
      halt0 = live.halt(listener, config.gracefulShutdownTimeout).withClock(clock)
      _ <- live
        .acceptAll(listener)(runConnection(routes, _, config, live.takingWork, _, tls.isDefined, originDate))
        .forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(listener.localPort, halt0)

  /** Node terminates TLS in its own listener, so a connection arrives already decrypted. */
  private def bind(config: Server.Config, tls: Option[Tls]): IO[ServerError, Listener[NodeConn]] =
    tls match
      case None    => NodeListener.plain(config)
      case Some(t) => t.listener(config)

  private def runConnection[R](
      routes: Routes[R, Response],
      conn: NodeConn,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean,
      originDate: OriginDate,
  ): ZIO[R, HttpError, Unit] =
    val readBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    Http1.serveConnection(
      routes,
      Wire(ConnBuf.fromConn(readBuf, conn), conn.write, secure),
      config,
      Lifecycle(takingWork, busy),
      originDate,
    )
end ServerPlatform
