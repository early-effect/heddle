package heddle.client

import heddle.BytesLength
import heddle.BytesLength.*
import heddle.endpoint.Endpoint
import heddle.error.HttpError
import heddle.http.{Body, Method, Request, Response, Url}
import heddle.http.header.Headers
import heddle.route.Routes
import heddle.sse.{ServerSentEvent, SseCodec}
import zio.*
import zio.stream.ZStream

/** An HTTP client. Transport failures are `ClientError`; a response with any status succeeds. */
trait Client:
  /** Sends `req` and reads the whole response body (at most `Config.maxBodyBytes`). */
  def batched(req: Request): IO[ClientError, Response]

  /** Sends `req` and returns once the head arrives. The body streams from the connection until `Scope` closes. */
  def streaming(req: Request): ZIO[Scope, ClientError, Response]

object Client:
  def batched(req: Request): ZIO[Client, ClientError, Response] =
    ZIO.serviceWithZIO(_.batched(req))

  def streaming(req: Request): ZIO[Client & Scope, ClientError, Response] =
    ZIO.serviceWithZIO[Client](_.streaming(req))

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

  /** The platform client, trusting the platform's certificate store. */
  def layer: ZLayer[Config, ClientError, Client] =
    ClientPlatform.layer

  val live: Layer[ClientError, Client] =
    ZLayer.succeed(Config.default) >>> layer

  /** One request on its own connection. */
  def get(url: String, config: Config = Config.default): IO[ClientError, Response] =
    decoded(url).flatMap(u => ClientPlatform.once(Request.get(u), config))

  /** One request on its own connection; `req`'s origin-form target is joined onto `base`. */
  def request(base: String, req: Request): IO[ClientError, Response] =
    request(base, req, Config.default)

  def request(base: String, req: Request, config: Config): IO[ClientError, Response] =
    decoded(base).flatMap(b => ClientPlatform.once(req.copy(url = join(b, req.url)), config))

  def request(
      method: Method,
      url: String,
      headers: Headers = Headers.empty,
      body: Body = Body.empty,
      config: Config = Config.default,
  ): IO[ClientError, Response] =
    decoded(url).flatMap(u => ClientPlatform.once(Request(method, u, headers, body), config))

  /** A `text/event-stream` GET. Anything but `200` fails the stream, as an `EventSource` would. */
  def sse(url: String, config: Config = Config.default): ZStream[Any, ClientError, ServerSentEvent] =
    ZStream.unwrapScoped {
      for
        req    <- decoded(url).map(u => Request.get(u))
        target <- ZIO.fromEither(Target.of(req))
        res    <- ClientPlatform.streaming(req, config)
        _      <- ZIO
          .fail(ClientError.Protocol(target.authority, HttpError.Malformed(s"SSE needs 200, got ${res.status.code}")))
          .unless(res.status.code == 200)
      yield res.body.toStream
        .mapError(ClientError.Io(target.authority, _))
        .chunks
        .mapAccum(Chunk.empty[Byte]) { (acc, chunk) =>
          val (events, rest) = SseCodec.decode(acc ++ chunk)
          (rest, events)
        }
        .flattenChunks
    }

  def inMemory[R](routes: Routes[R, Response]): URLayer[R, Client] =
    ZLayer.fromZIO(
      ZIO.environmentWith[R] { env =>
        new Client:
          def batched(req: Request): IO[ClientError, Response] =
            routes(req).provideEnvironment(env).merge

          def streaming(req: Request): ZIO[Scope, ClientError, Response] =
            batched(req)
      }
    )

  /** Pins the endpoint; `apply` takes its typed input. */
  def call[In, Err, Out](ep: Endpoint[In, Err, Out]): CallPartiallyApplied[In, Err, Out] =
    CallPartiallyApplied(ep)

  final class CallPartiallyApplied[In, Err, Out](ep: Endpoint[In, Err, Out]):
    def apply(in: In): ZIO[Client, CallFailure[Err], Out] =
      at(Url.root, in)

    def at(base: Url, in: In): ZIO[Client, CallFailure[Err], Out] =
      ZIO.fromEither(ep.toRequest(in, base)).orDieWith(IllegalArgumentException(_)).flatMap { req =>
        Client.batched(req).mapError(CallFailure.Transport(_)).flatMap(ep.fromResponse)
      }

  private def decoded(url: String): IO[ClientError, Url] =
    ZIO.fromEither(Url.decode(url)).mapError(ClientError.InvalidTarget(url, _))

  private def join(base: Url, rel: Url): Url =
    if rel.absolute then rel
    else base.copy(path = heddle.http.Path(base.path.segments ++ rel.path.segments), query = rel.query)
end Client
