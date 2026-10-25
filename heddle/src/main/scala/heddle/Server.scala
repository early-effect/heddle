package heddle

import heddle.error.ServerError
import heddle.http.Response
import heddle.route.Routes
import heddle.server.{
  Backlog,
  BodyBytes,
  ChunkSize,
  Configured,
  HeaderBytes,
  Http2Config,
  KeepAliveRequests,
  MaxConnections,
  Port,
  Tls,
}
import zio.*

final class Server private[heddle] (val port: UIO[Int], val shutdown: UIO[Unit])

object Server:
  final case class Config(
      host: String = Config.defaultHost,
      port: Port = Config.defaultPort,
      maxHeaderBytes: HeaderBytes = Config.defaultMaxHeaderBytes,
      maxBodyBytes: BodyBytes = Config.defaultMaxBodyBytes,
      chunkSize: ChunkSize = Config.defaultChunkSize,
      gracefulShutdownTimeout: Duration = Config.defaultGracefulShutdownTimeout,
      idleTimeout: Duration = Config.defaultIdleTimeout,
      headerTimeout: Duration = Config.defaultHeaderTimeout,
      maxConnections: MaxConnections = Config.defaultMaxConnections,
      maxRequestsPerConnection: Option[KeepAliveRequests] = Config.defaultMaxRequestsPerConnection,
      soBacklog: Backlog = Config.defaultSoBacklog,
      reuseAddress: Boolean = Config.defaultReuseAddress,
      tcpNoDelay: Boolean = Config.defaultTcpNoDelay,
      soKeepAlive: Boolean = Config.defaultSoKeepAlive,
      http2: Boolean = Config.defaultHttp2,
      http2Config: Http2Config = Http2Config(),
  ):
    def port(n: Port): Config = copy(port = n)

  object Config:
    val defaultHost: String                                        = "0.0.0.0"
    val defaultPort: Port                                          = Port(8080)
    val defaultMaxHeaderBytes: HeaderBytes                         = HeaderBytes(16 * 1024)
    val defaultMaxBodyBytes: BodyBytes                             = BodyBytes(10L * 1024 * 1024)
    val defaultChunkSize: ChunkSize                                = ChunkSize(8 * 1024)
    val defaultGracefulShutdownTimeout: Duration                   = 10.seconds
    val defaultIdleTimeout: Duration                               = 60.seconds
    val defaultHeaderTimeout: Duration                             = 30.seconds
    val defaultMaxConnections: MaxConnections                      = MaxConnections(1024)
    val defaultMaxRequestsPerConnection: Option[KeepAliveRequests] = None
    val defaultSoBacklog: Backlog                                  = Backlog(100)
    val defaultReuseAddress: Boolean                               = true
    val defaultTcpNoDelay: Boolean                                 = true
    val defaultSoKeepAlive: Boolean                                = true
    val defaultHttp2: Boolean                                      = true

    val default: Config = Config()

    val descriptor: zio.Config[Config] =
      (
        zio.Config.string("host").withDefault(defaultHost) ++
          Configured.int("port", defaultPort, _.toInt, Port.make) ++
          Configured.int("maxHeaderBytes", defaultMaxHeaderBytes, _.toInt, HeaderBytes.make) ++
          Configured.long("maxBodyBytes", defaultMaxBodyBytes, _.toLong, BodyBytes.make) ++
          Configured.int("chunkSize", defaultChunkSize, _.toInt, ChunkSize.make) ++
          zio.Config.duration("gracefulShutdownTimeout").withDefault(defaultGracefulShutdownTimeout) ++
          zio.Config.duration("idleTimeout").withDefault(defaultIdleTimeout) ++
          zio.Config.duration("headerTimeout").withDefault(defaultHeaderTimeout) ++
          Configured.int("maxConnections", defaultMaxConnections, _.toInt, MaxConnections.make) ++
          Configured.optionalInt("maxRequestsPerConnection", KeepAliveRequests.make) ++
          Configured.int("soBacklog", defaultSoBacklog, _.toInt, Backlog.make) ++
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

  /** HTTPS with the `Tls` the environment provides:
    * `Server.serveTls(app).provide(Config.defaults, Tls.pem(cert, key))`.
    */
  def serveTls[R](routes: Routes[R, Response]): ZIO[R & Config & Tls, ServerError, Nothing] =
    ZIO.serviceWithZIO[Config] { config =>
      ZIO.serviceWithZIO[Tls](tls => ZIO.scoped(install(routes, config, tls) *> ZIO.never))
    }

  def sbtInterruptExit: UIO[Unit] =
    ServerPlatform.sbtInterruptExit

  def install[R](routes: Routes[R, Response]): ZIO[R & Config & Scope, ServerError, Server] =
    ZIO.serviceWithZIO[Config](config => install(routes, config))

  def install[R](routes: Routes[R, Response], config: Config): ZIO[R & Scope, ServerError, Server] =
    ServerPlatform.install(routes, config, None)

  def install[R](routes: Routes[R, Response], config: Config, tls: Tls): ZIO[R & Scope, ServerError, Server] =
    ServerPlatform.install(routes, config, Some(tls))

  def run[R](routes: Routes[R, Response])(req: heddle.http.Request): ZIO[R, Nothing, Response] =
    routes(req).merge
end Server
