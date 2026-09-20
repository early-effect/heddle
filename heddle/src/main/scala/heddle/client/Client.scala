package heddle.client

import heddle.BytesLength
import heddle.BytesLength.*
import heddle.http.{Body, Method, Request, Response}
import heddle.http.header.Headers
import heddle.sse.ServerSentEvent
import zio.*
import zio.stream.ZStream

trait Client:
  def batched(req: Request): Task[Response]

object Client:
  def batched(req: Request): ZIO[Client, Throwable, Response] =
    ZIO.serviceWithZIO(_.batched(req))

  final case class Config(
      maxConnectionsPerHost: Int = Config.defaultMaxConnectionsPerHost,
      maxIdlePerHost: Int = Config.defaultMaxIdlePerHost,
      connectTimeout: Duration = Config.defaultConnectTimeout,
      idleTimeout: Duration = Config.defaultIdleTimeout,
      poolIdleTimeout: Duration = Config.defaultPoolIdleTimeout,
      addUserAgent: Boolean = Config.defaultAddUserAgent,
      maxHeaderBytes: BytesLength = Config.defaultMaxHeaderBytes,
      maxBodyBytes: BytesLength = Config.defaultMaxBodyBytes,
  )

  object Config:
    val defaultMaxConnectionsPerHost: Int  = 10
    val defaultMaxIdlePerHost: Int         = 10
    val defaultConnectTimeout: Duration    = 10.seconds
    val defaultIdleTimeout: Duration       = 60.seconds
    val defaultPoolIdleTimeout: Duration   = 60.seconds
    val defaultAddUserAgent: Boolean       = true
    val defaultMaxHeaderBytes: BytesLength = 64.K
    val defaultMaxBodyBytes: BytesLength   = 10.M

    val default: Config = Config()

    val descriptor: zio.Config[Config] =
      (
        zio.Config.int("maxConnectionsPerHost").withDefault(defaultMaxConnectionsPerHost) ++
          zio.Config.int("maxIdlePerHost").withDefault(defaultMaxIdlePerHost) ++
          zio.Config.duration("connectTimeout").withDefault(defaultConnectTimeout) ++
          zio.Config.duration("idleTimeout").withDefault(defaultIdleTimeout) ++
          zio.Config.duration("poolIdleTimeout").withDefault(defaultPoolIdleTimeout) ++
          zio.Config.boolean("addUserAgent").withDefault(defaultAddUserAgent) ++
          zio.Config.long("maxHeaderBytes").map(BytesLength(_)).withDefault(defaultMaxHeaderBytes) ++
          zio.Config.long("maxBodyBytes").map(BytesLength(_)).withDefault(defaultMaxBodyBytes)
      ).nested("heddle", "client").map {
        (
            maxConnectionsPerHost,
            maxIdlePerHost,
            connectTimeout,
            idleTimeout,
            poolIdleTimeout,
            addUserAgent,
            maxHeaderBytes,
            maxBodyBytes,
        ) =>
          Config(
            maxConnectionsPerHost = maxConnectionsPerHost,
            maxIdlePerHost = maxIdlePerHost,
            connectTimeout = connectTimeout,
            idleTimeout = idleTimeout,
            poolIdleTimeout = poolIdleTimeout,
            addUserAgent = addUserAgent,
            maxHeaderBytes = maxHeaderBytes,
            maxBodyBytes = maxBodyBytes,
          )
      }

    val layer: ZLayer[Any, zio.Config.Error, Config] =
      ZLayer(ZIO.config(descriptor))
  end Config

  def layer: ZLayer[Config, Nothing, Client] =
    ClientPlatform.layer

  val live: ULayer[Client] =
    ZLayer.succeed(Config.default) >>> layer

  def get(url: String, config: Config = Config.default): Task[Response] =
    ClientPlatform.get(url, config)

  def request(base: String, req: Request): Task[Response] =
    ClientPlatform.request(base, req)

  def request(base: String, req: Request, config: Config): Task[Response] =
    ClientPlatform.request(base, req, config)

  def request(
      method: Method,
      url: String,
      headers: Headers = Headers.empty,
      body: Body = Body.empty,
      config: Config = Config.default,
  ): Task[Response] =
    ClientPlatform.request(method, url, headers, body, config)

  def sse(url: String, config: Config = Config.default): ZStream[Any, Throwable, ServerSentEvent] =
    ClientPlatform.sse(url, config)
end Client
