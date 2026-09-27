package heddle.mcp.client

import heddle.LinePipe
import heddle.client.{Client, ClientError}
import heddle.http.Url
import heddle.http.header.Headers
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** Connects to MCP servers. The session negotiates the handshake: 2026-07-28 `server/discover` first, and 2025-11-25
  * `initialize` when the server does not speak the stateless revision.
  */
object McpClient:
  /** Who this client is, what it can do, and how long any one request may take. */
  final case class Settings(
      client: Implementation,
      capabilities: Json.Obj = Json.Obj(),
      requestTimeout: Duration = 30.seconds,
  )

  /** Streamable HTTP at `url`, over the `Client` in the environment. */
  def http(
      url: String,
      settings: Settings,
      headers: Headers = Headers.empty,
  ): ZIO[Scope & Client, McpError, McpSession] =
    for
      target  <- ZIO.fromEither(Url.decode(url)).mapError(r => McpError.Transport(ClientError.InvalidTarget(url, r)))
      client  <- ZIO.service[Client]
      sid     <- Ref.make(Option.empty[String])
      session <- connect(HttpCarrier(client, target, headers, sid), settings)
    yield session

  /** A server on the other end of a line pipe: a spawned process's stdio, or an in-memory pair in a test. */
  def pipe(pipe: LinePipe, settings: Settings): ZIO[Scope, McpError, McpSession] =
    PipeCarrier.scoped(pipe).flatMap(connect(_, settings))

  private def connect(carrier: Carrier, settings: Settings): IO[McpError, McpSession] =
    Ref.make(0L).flatMap { ids =>
      Live(carrier, Era.Stateless, None, ids, settings).request(ClientRequest.Discover).either.flatMap {
        case Right(result) => ZIO.succeed(Live(carrier, Era.Stateless, serverOf(result), ids, settings))
        case Left(e) if speaksOnlySession(e) => initialize(carrier, ids, settings)
        case Left(e)                         => ZIO.fail(e)
      }
    }

  private def initialize(carrier: Carrier, ids: Ref[Long], settings: Settings): IO[McpError, McpSession] =
    val opening = Live(carrier, Era.Session, None, ids, settings)
    for
      result <- opening.request(
        ClientRequest.Initialize(ProtocolVersion.Legacy, settings.capabilities, Some(settings.client))
      )
      _ <- carrier.notify(Message.Notification(Notifications.Initialized, Json.Obj()), Era.Session)
    yield Live(carrier, Era.Session, result.get("serverInfo").flatMap(_.as[Implementation].toOption), ids, settings)

  private def speaksOnlySession(e: McpError): Boolean =
    e match
      case McpError.Rpc(_: RpcError.MethodNotFound | _: RpcError.UnsupportedVersion | _: RpcError.InvalidParams) => true
      case McpError.Http(_)                                                                                      => true
      case _ => false

  private def serverOf(result: Json.Obj): Option[Implementation] =
    result
      .get("_meta")
      .collect { case o: Json.Obj => o }
      .flatMap(_.get(RequestMeta.ServerInfoKey))
      .flatMap(_.as[Implementation].toOption)

  private final class Live(
      carrier: Carrier,
      val era: Era,
      val server: Option[Implementation],
      ids: Ref[Long],
      settings: Settings,
  ) extends McpSession:
    def request(req: ClientRequest): IO[McpError, Json.Obj] =
      ids.updateAndGet(_ + 1).flatMap { n =>
        carrier
          .exchange(Message.in(era, RequestId.Num(n), req, settings.capabilities), era)
          .timeoutFail(McpError.TimedOut(req.method))(settings.requestTimeout)
          .flatMap {
            case Message.Result(_, r) => ZIO.succeed(r)
            case Message.Error(_, e)  => ZIO.fail(McpError.Rpc(e))
            case other                => ZIO.fail(McpError.Protocol(s"expected an answer, got ${other.json.toJson}"))
          }
      }
  end Live
end McpClient
