package heddle

import BytesLength.*
import heddle.error.{HttpError, ServerError}
import heddle.http.Response
import heddle.route.{Middleware, Routes}
import heddle.server.{Compressor, Http2Config}
import java.nio.channels.{ClosedChannelException, ServerSocketChannel, SocketChannel}
import java.net.StandardSocketOptions
import java.util.concurrent.ConcurrentHashMap
import heddle.internal.engine.{ConnBuf, Http1, Nio}
import zio.*

final class Server private (ss: ServerSocketChannel, halt: UIO[Unit]):
  def port: UIO[Int]      = ZIO.succeed(Nio.localPort(ss))
  def shutdown: UIO[Unit] = halt

object Server:
  final case class Config(
      host: String = Config.defaultHost,
      port: Int = Config.defaultPort,
      maxHeaderBytes: BytesLength = Config.defaultMaxHeaderBytes,
      maxBodyBytes: BytesLength = Config.defaultMaxBodyBytes,
      chunkSize: BytesLength = Config.defaultChunkSize,
      gracefulShutdownTimeout: Duration = Config.defaultGracefulShutdownTimeout,
      idleTimeout: Duration = Config.defaultIdleTimeout,
      headerTimeout: Duration = Config.defaultHeaderTimeout,
      maxConnections: Int = Config.defaultMaxConnections,
      maxRequestsPerConnection: Int = Config.defaultMaxRequestsPerConnection,
      soBacklog: Int = Config.defaultSoBacklog,
      reuseAddress: Boolean = Config.defaultReuseAddress,
      tcpNoDelay: Boolean = Config.defaultTcpNoDelay,
      soKeepAlive: Boolean = Config.defaultSoKeepAlive,
      http2: Boolean = Config.defaultHttp2,
      http2Config: Http2Config = Http2Config(),
      compressors: Chunk[Compressor] = Chunk.empty,
      compressMinBytes: Int = 0,
  ):
    def port(n: Int): Config = copy(port = n)
  end Config

  object Config:
    val defaultHost: String                      = "0.0.0.0"
    val defaultPort: Int                         = 8080
    val defaultMaxHeaderBytes: BytesLength       = 64.K
    val defaultMaxBodyBytes: BytesLength         = 10.M
    val defaultChunkSize: BytesLength            = 8.K
    val defaultGracefulShutdownTimeout: Duration = 10.seconds
    val defaultIdleTimeout: Duration             = 60.seconds
    val defaultHeaderTimeout: Duration           = 30.seconds
    val defaultMaxConnections: Int               = 1024
    val defaultMaxRequestsPerConnection: Int     = 10_000
    val defaultSoBacklog: Int                    = 100
    val defaultReuseAddress: Boolean             = true
    val defaultTcpNoDelay: Boolean               = true
    val defaultSoKeepAlive: Boolean              = true
    val defaultHttp2: Boolean                    = true

    val default: Config = Config()

    val descriptor: zio.Config[Config] =
      (
        zio.Config.string("host").withDefault(defaultHost) ++
          zio.Config.int("port").withDefault(defaultPort) ++
          zio.Config.long("maxHeaderBytes").map(BytesLength(_)).withDefault(defaultMaxHeaderBytes) ++
          zio.Config.long("maxBodyBytes").map(BytesLength(_)).withDefault(defaultMaxBodyBytes) ++
          zio.Config.long("chunkSize").map(BytesLength(_)).withDefault(defaultChunkSize) ++
          zio.Config.duration("gracefulShutdownTimeout").withDefault(defaultGracefulShutdownTimeout) ++
          zio.Config.duration("idleTimeout").withDefault(defaultIdleTimeout) ++
          zio.Config.duration("headerTimeout").withDefault(defaultHeaderTimeout) ++
          zio.Config.int("maxConnections").withDefault(defaultMaxConnections) ++
          zio.Config.int("maxRequestsPerConnection").withDefault(defaultMaxRequestsPerConnection) ++
          zio.Config.int("soBacklog").withDefault(defaultSoBacklog) ++
          zio.Config.boolean("reuseAddress").withDefault(defaultReuseAddress) ++
          zio.Config.boolean("tcpNoDelay").withDefault(defaultTcpNoDelay) ++
          zio.Config.boolean("soKeepAlive").withDefault(defaultSoKeepAlive) ++
          zio.Config.boolean("http2").withDefault(defaultHttp2) ++
          Http2Config.descriptor
      ).nested("heddle", "server").map {
        (
            host,
            port,
            maxHeaderBytes,
            maxBodyBytes,
            chunkSize,
            gracefulShutdownTimeout,
            idleTimeout,
            headerTimeout,
            maxConnections,
            maxRequestsPerConnection,
            soBacklog,
            reuseAddress,
            tcpNoDelay,
            soKeepAlive,
            http2,
            http2Config,
        ) =>
          Config(
            host = host,
            port = port,
            maxHeaderBytes = maxHeaderBytes,
            maxBodyBytes = maxBodyBytes,
            chunkSize = chunkSize,
            gracefulShutdownTimeout = gracefulShutdownTimeout,
            idleTimeout = idleTimeout,
            headerTimeout = headerTimeout,
            maxConnections = maxConnections,
            maxRequestsPerConnection = maxRequestsPerConnection,
            soBacklog = soBacklog,
            reuseAddress = reuseAddress,
            tcpNoDelay = tcpNoDelay,
            soKeepAlive = soKeepAlive,
            http2 = http2,
            http2Config = http2Config,
          )
      }

    val layer: ZLayer[Any, zio.Config.Error, Config] =
      ZLayer(ZIO.config(descriptor))

    val defaults: ULayer[Config] =
      ZLayer.succeed(default)
  end Config

  def defaultWith(f: Config => Config): ULayer[Config] =
    ZLayer.succeed(f(Config.default))

  /** Uses the fiber's Clock. `Duration.Infinity` and non-positive durations skip the clock. */
  private[heddle] def awaitWithin[E, A](d: Duration)(zio: IO[E, A]): IO[E, Option[A]] =
    if d == Duration.Infinity || d.toNanos <= 0L then zio.map(Some(_))
    else zio.timeout(d)

  def serve[R](routes: Routes[R, Response]): ZIO[R & Config, ServerError, Nothing] =
    ZIO.serviceWithZIO[Config](config => serve(routes, config))

  def serve[R](routes: Routes[R, Response], config: Config): ZIO[R, ServerError, Nothing] =
    ZIO.scoped(install(routes, config) *> ZIO.never)

  /** sbt 2 client-side run treats JVM SIGINT status 130 as a crash. Consume INT and exit 0; ZIOApp shutdown still
    * interrupts [[serve]].
    */
  def sbtInterruptExit: UIO[Unit] =
    ZIO.succeed {
      sun.misc.Signal.handle(sun.misc.Signal("INT"), _ => java.lang.System.exit(0))
      ()
    }

  def install[R](routes: Routes[R, Response]): ZIO[R & Config & Scope, ServerError, Server] =
    ZIO.serviceWithZIO[Config](config => install(routes, config))

  def install[R](routes: Routes[R, Response], config: Config): ZIO[R & Scope, ServerError, Server] =
    val app =
      if config.compressors.isEmpty then routes
      else routes @@ Middleware.compress(config.compressMinBytes, config.compressors)
    for
      _          <- loom.build.unit
      clock      <- ZIO.clock
      tls        <- ZIO.environmentWith[R & Scope](_.getDynamic[Tls])
      takingWork <- ZIO.succeed(java.util.concurrent.atomic.AtomicBoolean(true))
      live       <- ZIO.succeed(ConcurrentHashMap.newKeySet[Conn]())
      inflight   <- ZIO.succeed(java.util.concurrent.atomic.AtomicInteger(0))
      ss         <- ZIO.acquireRelease(Nio.openServer(config))(ss => ZIO.succeed(closeQuietly(ss)))
      halt0 = halt(ss, live, takingWork, config.gracefulShutdownTimeout).withClock(clock)
      _ <- acceptLoop(app, ss, config, live, inflight, takingWork, tls).forkScoped
      _ <- ZIO.addFinalizer(halt0)
    yield Server(ss, halt0)
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
      config: Config,
      live: java.util.Set[Conn],
      inflight: java.util.concurrent.atomic.AtomicInteger,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, Nothing, Nothing] =
    acceptOne(routes, ss, config, live, inflight, takingWork, tls).forever

  private def acceptOne[R](
      routes: Routes[R, Response],
      ss: ServerSocketChannel,
      config: Config,
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
      config: Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      tls: Option[Tls],
  ): ZIO[R, HttpError, Unit] =
    val readBuf  = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    val writeBuf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, 4096))
    tls match
      case None =>
        val src  = ConnBuf.channel(readBuf, ch)
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
      config: Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean,
  ): ZIO[R, HttpError, Unit] =
    if !config.http2 then Http1.serveConnection(routes, src, send, config, takingWork, busy, secure)
    else
      val preface = heddle.internal.h2.H2Frame.Preface
      src.setReadTimeout(config.headerTimeout) *>
        awaitWithin(config.headerTimeout)(src.fillUntil(preface.length)).flatMap {
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
end Server
