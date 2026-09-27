package heddle.mcp.server

import heddle.http.header.Headers
import heddle.mcp.protocol.*
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, ZIO}

/** The handshake a request arrived under. */
enum Era:
  /** 2026-07-28: `server/discover`, a protocol version in every request's `_meta`, `complete` results. */
  case Stateless

  /** 2025-11-25: `initialize`, then a session; results without the 2026 envelope. */
  case Session

/** Answers MCP requests from typed messages. Transports parse bytes and pick the [[Era]]; this decides everything else,
  * and never throws or dies on what a client sends.
  */
final class Engine[-R](
    val server: Implementation,
    val tools: Chunk[ToolCall[R]],
    val instructions: Option[String],
    val listTtlMs: Long,
):
  private val byName: Map[ToolName, ToolCall[R]] = tools.map(t => t.tool.name -> t).toMap

  val capabilities: Json.Obj = Json.Obj("tools" -> Json.Obj())

  /** Parses and answers one JSON value. Notifications and stray responses get no reply. */
  def handle(raw: Json, headers: Headers, era: Era): ZIO[R, Nothing, Option[Message]] =
    Message.decode(raw).fold(e => ZIO.some(e), respond(_, headers, era))

  def respond(msg: Message, headers: Headers, era: Era): ZIO[R, Nothing, Option[Message]] =
    msg match
      case Message.Request(id, method, params) => answer(method, params, headers, era).map(reply(id, era, _)).asSome
      case _                                   => ZIO.none

  private def reply(id: RequestId, era: Era, out: Either[RpcError, Json.Obj]): Message =
    out match
      case Left(e)     => Message.Error(Some(id), e)
      case Right(body) =>
        era match
          case Era.Stateless => Message.Result(id, Envelope.withServer(body, server))
          case Era.Session   => Message.Result(id, Envelope.forSession(body))

  private def answer(
      method: String,
      params: Json.Obj,
      headers: Headers,
      era: Era,
  ): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    admit(params, era).flatMap(_ => ClientRequest.decode(method, params)) match
      case Left(e)    => ZIO.left(e)
      case Right(req) => serve(req, headers, era)

  /** 2026 requests must name the one revision this server speaks. */
  private def admit(params: Json.Obj, era: Era): Either[RpcError, Unit] =
    era match
      case Era.Session   => Right(())
      case Era.Stateless =>
        RequestMeta.of(params).protocolVersion match
          case None                                    => Left(RpcError.InvalidParams("missing protocol version"))
          case Some(v) if v != ProtocolVersion.Current =>
            Left(RpcError.UnsupportedVersion(v.value, Chunk(ProtocolVersion.Current)))
          case Some(_) => Right(())

  private def serve(req: ClientRequest, headers: Headers, era: Era): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    (req, era) match
      case (ClientRequest.Discover, Era.Stateless)          => ZIO.right(discover)
      case (ClientRequest.Initialize(_, _, _), Era.Session) =>
        ZIO.right(Envelope.initialize(server, capabilities, instructions))
      case (ClientRequest.Ping, _)                 => ZIO.right(Envelope.complete())
      case (ClientRequest.ListTools(_), _)         => ZIO.right(listTools)
      case (ClientRequest.CallTool(name, args), _) =>
        byName.get(name) match
          case None       => ZIO.left(RpcError.InvalidParams(s"Unknown tool: ${name.value}"))
          case Some(tool) => tool.call(args, headers).map(r => Right(Envelope.complete(encoded(r))))
      case (other, _) => ZIO.left(RpcError.methodNotFound(other.method))

  private def discover: Json.Obj =
    Envelope.complete(
      Chunk(
        "supportedVersions" -> Json.Arr(Json.Str(ProtocolVersion.Current.value)),
        "capabilities"      -> capabilities,
        "ttlMs"             -> Json.Num(listTtlMs),
        "cacheScope"        -> Json.Str("public"),
      ) ++ Chunk.fromIterable(instructions.map("instructions" -> Json.Str(_)))*
    )

  private def listTools: Json.Obj =
    Envelope.complete(
      "tools"      -> Json.Arr(tools.map(t => encoded(t.tool))),
      "ttlMs"      -> Json.Num(listTtlMs),
      "cacheScope" -> Json.Str("public"),
    )

  private def encoded[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST match
      case Right(o: Json.Obj) => o
      case _                  => Json.Obj()
end Engine
