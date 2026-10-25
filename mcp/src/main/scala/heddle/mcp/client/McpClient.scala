package heddle.mcp.client

import heddle.LinePipe
import heddle.client.{Client, ClientError, TargetError}
import heddle.http.Url
import heddle.http.header.Headers
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** Connects to MCP servers. The session negotiates the handshake: 2026-07-28 `server/discover` first, and 2025-11-25
  * `initialize` when the server does not speak the stateless revision.
  */
object McpClient:
  /** Who this client is, what it can do, and how long any one request may take. */
  /** `Discover` keeps today's handshake. `Session` is the 2025-11-25 session, including its GET stream. */
  enum Handshake:
    case Discover, Session

  final case class Settings(
      client: Implementation,
      capabilities: Json.Obj = Json.Obj(),
      requestTimeout: Duration = 30.seconds,
      handshake: Handshake = Handshake.Discover,
  )

  /** Streamable HTTP at `url`, over the `Client` in the environment. */
  def http(
      url: String,
      settings: Settings,
      headers: Headers = Headers.empty,
  ): ZIO[Scope & Client, McpError, McpSession] =
    for
      target <- ZIO
        .fromEither(Url.decode(url))
        .mapError(e => McpError.Transport(ClientError.InvalidTarget(url, TargetError.Malformed(e))))
      client  <- ZIO.service[Client]
      carrier <- HttpCarrier.make(client, target, headers)
      session <- connect(carrier, settings)
    yield session

  /** A server on the other end of a line pipe: a spawned process's stdio, or an in-memory pair in a test. */
  def pipe(pipe: LinePipe, settings: Settings): ZIO[Scope, McpError, McpSession] =
    PipeCarrier.scoped(pipe).flatMap(connect(_, settings))

  private def connect(carrier: Carrier, settings: Settings): ZIO[Scope, McpError, McpSession] =
    ZIO.scopeWith { scope =>
      (Ref.make(0L) <*> Hub.sliding[Message.Notification](64)).flatMap { (ids, notes) =>
        carrier.attach(notes, scope) *>
          (settings.handshake match
            case Handshake.Discover =>
              Live(carrier, Era.Stateless, None, ids, settings, notes).request(ClientRequest.Discover).either.flatMap {
                case Right(result) => ZIO.succeed(Live(carrier, Era.Stateless, serverOf(result), ids, settings, notes))
                case Left(e) if speaksOnlySession(e) => initialize(carrier, ids, settings, notes)
                case Left(e)                         => ZIO.fail(e)
              }
            case Handshake.Session => initialize(carrier, ids, settings, notes))
      }
    }

  private def initialize(
      carrier: Carrier,
      ids: Ref[Long],
      settings: Settings,
      notes: Hub[Message.Notification],
  ): ZIO[Scope, McpError, McpSession] =
    val opening = Live(carrier, Era.Session, None, ids, settings, notes)
    for
      result <- opening.request(
        ClientRequest.Initialize(ProtocolVersion.Legacy, settings.capabilities, Some(settings.client))
      )
      _ <- carrier.notify(Message.Notification(Notifications.Initialized, Json.Obj()), Era.Session)
      _ <- carrier.openEvents(notes)
    yield Live(
      carrier,
      Era.Session,
      result.get("serverInfo").flatMap(_.as[Implementation].toOption),
      ids,
      settings,
      notes,
    )

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
      notes: Hub[Message.Notification],
  ) extends McpSession:
    override def notifications: ZStream[Any, McpError, Message.Notification] = ZStream.fromHub(notes)

    def open(req: ClientRequest): IO[McpError, McpSession.OpenCall] =
      for
        n <- ids.updateAndGet(_ + 1)
        id = RequestId.Num(n)
        answer <- Promise.make[McpError, Message]
        seen   <- Ref.make(Chunk.empty[Message.Notification])
        _      <- carrier.submit(Message.in(era, id, req, settings.capabilities), era, Inflight(answer, seen))
      yield new McpSession.OpenCall(
        id,
        outcome(answer.await).ensuring(carrier.forget(id)),
        seen.get,
        cancel(id, answer),
      )

    def request(req: ClientRequest): IO[McpError, Json.Obj] =
      open(req).flatMap(_.result).timeoutFail(McpError.TimedOut(req.method))(settings.requestTimeout)

    private def cancel(id: RequestId, answer: Promise[McpError, Message]): IO[McpError, Unit] =
      // Fail the waiter first. The server then closing this call's stream must not replace `Cancelled`.
      answer.fail(McpError.Cancelled(id)) *>
        carrier.notify(Cancelled(id, None).message, era).ensuring(carrier.forget(id))

    private def outcome(message: IO[McpError, Message]): IO[McpError, Json.Obj] =
      message.flatMap {
        case Message.Result(_, result) => ZIO.succeed(result)
        case Message.Error(_, error)   => ZIO.fail(McpError.Rpc(error))
        case other                     => ZIO.fail(McpError.Protocol(s"expected an answer, got ${other.json.toJson}"))
      }
  end Live
end McpClient
