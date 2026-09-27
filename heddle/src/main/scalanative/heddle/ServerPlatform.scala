package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.internal.duplex.{NativeConn, NativeListener}
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
      clock    <- ZIO.clock
      live     <- LiveConnections.make(config.maxConnections)
      listener <- NativeListener.bind(config)
      port     <- listener.localPort
      halt0 = live.halt(listener, config.gracefulShutdownTimeout).withClock(clock)
      _ <- live.acceptAll(listener)(runConnection(routes, _, config, live.takingWork, _, tls)).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(ZIO.succeed(port), halt0)
    end for
  end install

  private def runConnection[R](
      routes: Routes[R, Response],
      conn: NativeConn,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, HttpError, Unit] =
    val readBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    val life    = Lifecycle(takingWork, busy)
    tls match
      case None =>
        Http1.serveConnection(routes, Wire(ConnBuf.fromConn(readBuf, conn), conn.write, secure = false), config, life)
      case Some(t) =>
        t.server(conn).flatMap { session =>
          val wire = Wire(ConnBuf.fromConn(readBuf, session.conn), session.conn.write, secure = true)
          Http1.serveConnection(routes, wire, config, life).ensuring(session.conn.close)
        }
    end match
  end runConnection
end ServerPlatform
