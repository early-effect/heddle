package heddle

import BytesLength.*
import heddle.error.ServerError
import heddle.http.Response
import heddle.route.Routes
import heddle.server.Http2Config
import zio.*

final class Server private[heddle] (val port: UIO[Int], val shutdown: UIO[Unit])

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

  def sbtInterruptExit: UIO[Unit] =
    ServerPlatform.sbtInterruptExit

  def install[R](routes: Routes[R, Response]): ZIO[R & Config & Scope, ServerError, Server] =
    ZIO.serviceWithZIO[Config](config => install(routes, config))

  def install[R](routes: Routes[R, Response], config: Config): ZIO[R & Scope, ServerError, Server] =
    ServerPlatform.install(routes, config)
end Server
