package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.internal.duplex.{ByteConn, ChannelListener, Listener}
import heddle.internal.engine.{ConnBuf, Http1}
import heddle.route.Routes
import heddle.server.Tls
import java.nio.channels.ClosedChannelException
import java.util.concurrent.ConcurrentHashMap
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

  def install[R](routes: Routes[R, Response], config: Server.Config): ZIO[R & Scope, ServerError, Server] =
    install(routes, config, JvmScheduler.Loom)

  def install[R](
      routes: Routes[R, Response],
      config: Server.Config,
      scheduler: JvmScheduler,
  ): ZIO[R & Scope, ServerError, Server] =
    for
      _          <- enableScheduler(scheduler)
      clock      <- ZIO.clock
      tls        <- ZIO.environmentWith[R & Scope](_.getDynamic[Tls])
      takingWork <- ZIO.succeed(java.util.concurrent.atomic.AtomicBoolean(true))
      live       <- ZIO.succeed(ConcurrentHashMap.newKeySet[Conn]())
      inflight   <- ZIO.succeed(java.util.concurrent.atomic.AtomicInteger(0))
      listener   <- ZIO.acquireRelease(ChannelListener.bind(config))(_.close)
      halt0 = halt(listener, live, takingWork, config.gracefulShutdownTimeout).withClock(clock)
      _ <- acceptLoop(routes, listener, config, live, inflight, takingWork, tls).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(listener.localPort, halt0)
    end for
  end install

  private def enableScheduler(scheduler: JvmScheduler): URIO[Scope, Unit] =
    scheduler match
      case JvmScheduler.Default => ZIO.unit
      case JvmScheduler.Loom    =>
        (Runtime.enableLoomBasedExecutor ++ Runtime.enableLoomBasedBlockingExecutor).build.unit.ignore

  /** Live connection. `fiber` is set after `forkDaemon`; halt interrupts whatever is still in `live`. */
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

  /** `Schedule.upTo`, not `timeout`: halt is a Scope finalizer (uninterruptible). */
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
        if e.isInstanceOf[ClosedChannelException] then ZIO.interrupt
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
      else ZIO.fail(HttpError.Malformed("expected h2 preface after ALPN h2"))
    }
end ServerPlatform
