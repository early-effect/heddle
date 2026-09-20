package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.internal.duplex.{ByteConn, Listener, NativeListener}
import heddle.internal.engine.{ConnBuf, Http1}
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
      clock      <- ZIO.clock
      tls        <- ZIO.environmentWith[R & Scope](_.getDynamic[Tls])
      takingWork <- ZIO.succeed(java.util.concurrent.atomic.AtomicBoolean(true))
      live       <- ZIO.succeed(java.util.HashSet[Conn]())
      inflight   <- ZIO.succeed(java.util.concurrent.atomic.AtomicInteger(0))
      listener   <- ZIO.acquireRelease(NativeListener.bind(config))(_.close)
      port       <- listener.localPort
      halt0 = halt(listener, live, takingWork, config.gracefulShutdownTimeout).withClock(clock)
      _ <- acceptLoop(routes, listener, config, live, inflight, takingWork, tls).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(ZIO.succeed(port), halt0)
    end for
  end install

  private final class Conn(
      val conn: ByteConn,
      val busy: java.util.concurrent.atomic.AtomicBoolean,
      val fiber: java.util.concurrent.atomic.AtomicReference[Fiber[Any, Any]],
  )

  private def halt(
      listener: Listener,
      live: java.util.Set[Conn],
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      grace: Duration,
  ): UIO[Unit] =
    ZIO.succeed(takingWork.set(false)) *>
      listener.close *>
      closeIdle(live) *>
      waitUntilIdle(live, grace) *>
      closeAll(live) *>
      interruptLive(live)

  private def interruptLive(live: java.util.Set[Conn]): UIO[Unit] =
    ZIO.suspendSucceed {
      val it = live.iterator()
      val fs = scala.collection.mutable.ArrayBuffer.empty[Fiber[Any, Any]]
      while it.hasNext do
        val f = it.next().fiber.get()
        if f != null then fs += f
      ZIO.foreachParDiscard(fs)(_.interrupt)
    }

  private def anyBusy(live: java.util.Set[Conn]): Boolean =
    val it = live.iterator()
    while it.hasNext do if it.next().busy.get() then return true
    false

  private def closeIdle(live: java.util.Set[Conn]): UIO[Unit] =
    ZIO.suspendSucceed {
      val it = live.iterator()
      val cs = scala.collection.mutable.ArrayBuffer.empty[ByteConn]
      while it.hasNext do
        val c = it.next()
        if !c.busy.get() then cs += c.conn
      ZIO.foreachDiscard(cs)(_.close)
    }

  private def closeAll(live: java.util.Set[Conn]): UIO[Unit] =
    ZIO.suspendSucceed {
      val it = live.iterator()
      val cs = scala.collection.mutable.ArrayBuffer.empty[ByteConn]
      while it.hasNext do cs += it.next().conn
      ZIO.foreachDiscard(cs)(_.close)
    }

  private def waitUntilIdle(live: java.util.Set[Conn], grace: Duration): UIO[Unit] =
    if grace.isZero then ZIO.unit
    else
      ZIO
        .succeed(anyBusy(live))
        .repeat(
          Schedule.spaced(10.millis) && Schedule.recurWhile[Boolean](identity) && Schedule.upTo(grace)
        )
        .unit

  private def acceptLoop[R](
      routes: Routes[R, Response],
      listener: Listener,
      config: Server.Config,
      live: java.util.Set[Conn],
      inflight: java.util.concurrent.atomic.AtomicInteger,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, Nothing, Nothing] =
    acceptOne(routes, listener, config, live, inflight, takingWork, tls).forever

  private def acceptOne[R](
      routes: Routes[R, Response],
      listener: Listener,
      config: Server.Config,
      live: java.util.Set[Conn],
      inflight: java.util.concurrent.atomic.AtomicInteger,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, Nothing, Unit] =
    listener.accept.foldZIO(
      e =>
        if e.getMessage == "listener closed" then ZIO.interrupt
        else ZIO.logWarning(e.toString).unit,
      byteConn =>
        if inflight.incrementAndGet() > config.maxConnections then
          inflight.decrementAndGet()
          byteConn.close
        else
          val busy     = java.util.concurrent.atomic.AtomicBoolean(false)
          val fiberRef = java.util.concurrent.atomic.AtomicReference[Fiber[Any, Any]]()
          val conn     = Conn(byteConn, busy, fiberRef)
          val started  = for
            _     <- ZIO.succeed { live.add(conn); () }
            fiber <- runConnection(routes, byteConn, config, takingWork, busy, tls)
              .ensuring(
                ZIO.succeed {
                  live.remove(conn)
                  inflight.decrementAndGet()
                  ()
                } *> byteConn.close
              )
              .forkDaemon
            _ <- ZIO.succeed { fiberRef.set(fiber); () }
          yield ()
          started,
    )

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
