package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.internal.duplex.{ByteConn, NativeListener}
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
      tls      <- ZIO.environmentWith[Any](_.getDynamic[Tls])
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
        Http1.serveConnection(routes, src, send, config, takingWork, busy, secure = false)
      case Some(t) =>
        t.server(conn, Chunk.empty).flatMap { session =>
          val src  = ConnBuf.fromConn(readBuf, session.conn)
          val send = session.conn.write
          Http1
            .serveConnection(routes, src, send, config, takingWork, busy, secure = true)
            .ensuring(session.conn.close)
        }
    end match
  end runConnection
end ServerPlatform
