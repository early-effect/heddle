package heddle

import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.route.Routes
import heddle.server.Tls
import java.nio.channels.{ClosedChannelException, ServerSocketChannel, SocketChannel}
import java.net.StandardSocketOptions
import java.util.concurrent.ConcurrentHashMap
import heddle.internal.engine.{ConnBuf, ConnBufPlatform, Http1, Nio}
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
    for
      _          <- loom.build.unit
      clock      <- ZIO.clock
      tls        <- ZIO.environmentWith[R & Scope](_.getDynamic[Tls])
      takingWork <- ZIO.succeed(java.util.concurrent.atomic.AtomicBoolean(true))
      live       <- ZIO.succeed(ConcurrentHashMap.newKeySet[Conn]())
      inflight   <- ZIO.succeed(java.util.concurrent.atomic.AtomicInteger(0))
      ss         <- ZIO.acquireRelease(Nio.openServer(config))(ss => ZIO.succeed(closeQuietly(ss)))
      halt0 = halt(ss, live, takingWork, config.gracefulShutdownTimeout).withClock(clock)
      _ <- acceptLoop(routes, ss, config, live, inflight, takingWork, tls).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(ZIO.succeed(Nio.localPort(ss)), halt0)
    end for
  end install

  private val loom: ZLayer[Any, ServerError, Unit] =
    (Runtime.enableLoomBasedExecutor ++ Runtime.enableLoomBasedBlockingExecutor)
      .mapError(e => ServerError.LoomUnavailable(e))

  /** Live connection. `fiber` is set after `forkDaemon`; halt interrupts whatever is still in `live`. */
  private final class Conn(
      val ch: SocketChannel,
      val busy: java.util.concurrent.atomic.AtomicBoolean,
      val fiber: java.util.concurrent.atomic.AtomicReference[Fiber[Any, Any]],
  )

  private def halt(
      ss: ServerSocketChannel,
      live: java.util.Set[Conn],
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      grace: Duration,
  ): UIO[Unit] =
    ZIO.succeed(takingWork.set(false)) *>
      ZIO.succeed(closeQuietly(ss)) *>
      ZIO.succeed(closeIdle(live)) *>
      waitUntilIdle(live, grace) *>
      ZIO.succeed(live.forEach(c => closeQuietly(c.ch))) *>
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

  private def closeIdle(live: java.util.Set[Conn]): Unit =
    val it = live.iterator()
    while it.hasNext do
      val c = it.next()
      if !c.busy.get() then closeQuietly(c.ch)

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

  private def closeQuietly(resource: AutoCloseable): Unit =
    try resource.close()
    catch case _: Throwable => ()

  private def acceptLoop[R](
      routes: Routes[R, Response],
      ss: ServerSocketChannel,
      config: Server.Config,
      live: java.util.Set[Conn],
      inflight: java.util.concurrent.atomic.AtomicInteger,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, Nothing, Nothing] =
    acceptOne(routes, ss, config, live, inflight, takingWork, tls).forever

  private def acceptOne[R](
      routes: Routes[R, Response],
      ss: ServerSocketChannel,
      config: Server.Config,
      live: java.util.Set[Conn],
      inflight: java.util.concurrent.atomic.AtomicInteger,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, Nothing, Unit] =
    Nio
      .accept(ss)
      .foldZIO(
        e =>
          if !ss.isOpen || e.isInstanceOf[ClosedChannelException] then ZIO.interrupt
          else ZIO.logWarning(e.toString).unit,
        ch =>
          if inflight.incrementAndGet() > config.maxConnections then
            ZIO.succeed {
              inflight.decrementAndGet()
              closeQuietly(ch)
            }
          else
            for
              _ <- ZIO.attempt(ch.setOption(StandardSocketOptions.TCP_NODELAY, config.tcpNoDelay)).ignore
              _ <- ZIO.attempt(ch.setOption(StandardSocketOptions.SO_KEEPALIVE, config.soKeepAlive)).ignore
              busy     = java.util.concurrent.atomic.AtomicBoolean(false)
              fiberRef = java.util.concurrent.atomic.AtomicReference[Fiber[Any, Any]]()
              conn     = Conn(ch, busy, fiberRef)
              _     <- ZIO.succeed { live.add(conn); () }
              fiber <- runConnection(routes, ch, config, takingWork, busy, tls)
                .ensuring(
                  ZIO.succeed {
                    live.remove(conn)
                    inflight.decrementAndGet()
                    ()
                  } *> ZIO.succeed(closeQuietly(ch))
                )
                .forkDaemon // not a child of accept: interrupting accept must not abort in-flight work
              _ <- ZIO.succeed { fiberRef.set(fiber); () }
            yield (),
      )

  private def runConnection[R](
      routes: Routes[R, Response],
      ch: SocketChannel,
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, HttpError, Unit] =
    val readBuf  = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    val writeBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, 4096))
    tls match
      case None =>
        val src  = ConnBufPlatform.channel(readBuf, ch)
        val send = Nio.writer(ch, writeBuf)
        serve(routes, src, send, config, takingWork, busy, secure = false)
      case Some(t) =>
        val alpn = if config.http2 then Chunk("h2", "http/1.1") else Chunk("http/1.1")
        t.wrap(ch, alpn).flatMap { session =>
          val src   = session.src(readBuf)
          val send  = session.send
          val proto = session.applicationProtocol
          val work  =
            if proto == "h2" && config.http2 then
              consumePreface(src) *>
                heddle.internal.h2.H2Connection.serve(routes, src, send, config, takingWork, busy, secure = true)
            else Http1.serveConnection(routes, src, send, config, takingWork, busy, secure = true)
          work.ensuring(ZIO.succeed(closeQuietly(session.socket)))
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
