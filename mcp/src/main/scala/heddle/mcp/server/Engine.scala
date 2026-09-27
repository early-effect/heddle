package heddle.mcp.server

import heddle.http.header.Headers
import heddle.mcp.ServedResource
import heddle.mcp.protocol.*
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, ZIO}

/** What a server offers: tools, resources, and the extensions it advertises with their settings. */
final case class Offer[-R](
    tools: Chunk[ToolCall[R]],
    resources: Chunk[ServedResource[R]],
    extensions: Map[ExtensionId, Json.Obj],
)

/** Answers MCP requests from typed messages. Transports parse bytes and pick the [[Era]]; this decides everything else,
  * and never throws or dies on what a client sends.
  */
final class Engine[-R](
    val server: Implementation,
    val offer: Offer[R],
    val instructions: Option[String],
    val listTtlMs: Long,
):
  private val byName: Map[ToolName, ToolCall[R]]    = offer.tools.map(t => t.tool.name -> t).toMap
  private val byUri: Map[String, ServedResource[R]] = offer.resources.map(r => r.resource.uri -> r).toMap

  /** `tools` always; `resources` when there are any; `extensions` as `server/discover` and `initialize` send them. */
  val capabilities: Json.Obj =
    val resources  = Option.when(offer.resources.nonEmpty)("resources" -> Json.Obj())
    val extensions = Option.when(offer.extensions.nonEmpty)(
      "extensions" -> Json.Obj(Chunk.fromIterable(offer.extensions.map((id, settings) => id.value -> settings)))
    )
    Json.Obj(Chunk("tools" -> Json.Obj()) ++ Chunk.fromIterable(resources) ++ Chunk.fromIterable(extensions))

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
      case (ClientRequest.ListResources(_), _) =>
        ZIO.right(listed("resources", Json.Arr(offer.resources.map(r => encoded(r.resource)))))
      case (ClientRequest.ListResourceTemplates(_), _) =>
        ZIO.right(listed("resourceTemplates", Json.Arr()))
      case (ClientRequest.ReadResource(uri), _) =>
        byUri.get(uri) match
          case None         => ZIO.left(RpcError.ResourceNotFound(s"Resource not found: $uri"))
          case Some(served) =>
            served.read.fold(
              e => Left(RpcError.Internal(s"Resource $uri is unavailable: ${e.reason}")),
              contents => Right(Envelope.complete(encoded(ReadResourceResult(contents)))),
            )
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
    listed("tools", Json.Arr(offer.tools.map(t => encoded(t.tool))))

  private def listed(key: String, items: Json.Arr): Json.Obj =
    Envelope.complete(key -> items, "ttlMs" -> Json.Num(listTtlMs), "cacheScope" -> Json.Str("public"))

  private def encoded[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST match
      case Right(o: Json.Obj) => o
      case _                  => Json.Obj()
end Engine
