package heddle.mcp.protocol

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** What a client asks a server. Unknown methods and malformed params are `RpcError`s, never exceptions. */
enum ClientRequest:
  case Ping
  case Discover
  case Initialize(protocolVersion: ProtocolVersion, capabilities: Json.Obj, clientInfo: Option[Implementation])
  case ListTools(cursor: Option[String])
  case CallTool(name: ToolName, arguments: Json.Obj)
  case ListResources(cursor: Option[String])
  case ListResourceTemplates(cursor: Option[String])
  case ReadResource(uri: String)

  def method: String =
    this match
      case Ping                     => Methods.Ping
      case Discover                 => Methods.Discover
      case Initialize(_, _, _)      => Methods.Initialize
      case ListTools(_)             => Methods.ListTools
      case CallTool(_, _)           => Methods.CallTool
      case ListResources(_)         => Methods.ListResources
      case ListResourceTemplates(_) => Methods.ListResourceTemplates
      case ReadResource(_)          => Methods.ReadResource

  def params: Json.Obj =
    def cursor(c: Option[String]) = Json.Obj(Chunk.fromIterable(c.map("cursor" -> Json.Str(_))))
    this match
      case Ping | Discover           => Json.Obj()
      case Initialize(v, caps, info) =>
        Json.Obj(
          Chunk("protocolVersion" -> Json.Str(v.value), "capabilities" -> caps) ++
            Chunk.fromIterable(info.flatMap(_.toJsonAST.toOption).map("clientInfo" -> _))
        )
      case ListTools(c)             => cursor(c)
      case CallTool(n, args)        => Json.Obj("name" -> Json.Str(n.value), "arguments" -> args)
      case ListResources(c)         => cursor(c)
      case ListResourceTemplates(c) => cursor(c)
      case ReadResource(uri)        => Json.Obj("uri" -> Json.Str(uri))
    end match
  end params
end ClientRequest

/** Wire notification names. */
object Notifications:
  val Initialized = "notifications/initialized"

/** Wire method names. */
object Methods:
  val Ping                  = "ping"
  val Discover              = "server/discover"
  val Initialize            = "initialize"
  val ListTools             = "tools/list"
  val CallTool              = "tools/call"
  val ListResources         = "resources/list"
  val ListResourceTemplates = "resources/templates/list"
  val ReadResource          = "resources/read"

  val requests: Set[String] =
    Set(Ping, Discover, Initialize, ListTools, CallTool, ListResources, ListResourceTemplates, ReadResource)
end Methods

object ClientRequest:
  def known(method: String): Boolean = Methods.requests.contains(method)

  def decode(method: String, params: Json.Obj): Either[RpcError, ClientRequest] =
    method match
      case Methods.Ping                  => Right(Ping)
      case Methods.Discover              => Right(Discover)
      case Methods.Initialize            => initialize(params)
      case Methods.ListTools             => cursor(params).map(ListTools(_))
      case Methods.CallTool              => callTool(params)
      case Methods.ListResources         => cursor(params).map(ListResources(_))
      case Methods.ListResourceTemplates => cursor(params).map(ListResourceTemplates(_))
      case Methods.ReadResource          =>
        params.get("uri") match
          case Some(Json.Str(u)) => Right(ReadResource(u))
          case _                 => Left(RpcError.InvalidParams("resources/read needs a string uri"))
      case other => Left(RpcError.methodNotFound(other))

  private def initialize(p: Json.Obj): Either[RpcError, ClientRequest] =
    val caps = p.get("capabilities") match
      case Some(o: Json.Obj) => Right(o)
      case None              => Right(Json.Obj())
      case Some(_)           => Left(RpcError.InvalidParams("capabilities must be an object"))
    for
      version <- p.get("protocolVersion") match
        case Some(Json.Str(v)) => Right(ProtocolVersion(v))
        case _                 => Left(RpcError.InvalidParams("initialize needs a string protocolVersion"))
      c    <- caps
      info <- p.get("clientInfo") match
        case None    => Right(None)
        case Some(j) => j.as[Implementation].map(Some(_)).left.map(e => RpcError.InvalidParams(s"clientInfo: $e"))
    yield Initialize(version, c, info)
  end initialize

  private def callTool(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name <- p.get("name") match
        case Some(Json.Str(n)) => ToolName.from(n).left.map(e => RpcError.InvalidParams(e.message))
        case _                 => Left(RpcError.InvalidParams("missing tool name"))
      args <- p.get("arguments") match
        case None              => Right(Json.Obj())
        case Some(o: Json.Obj) => Right(o)
        case Some(_)           => Left(RpcError.InvalidParams("arguments must be a JSON object"))
    yield CallTool(name, args)

  private def cursor(p: Json.Obj): Either[RpcError, Option[String]] =
    p.get("cursor") match
      case None              => Right(None)
      case Some(Json.Str(c)) => Right(Some(c))
      case Some(_)           => Left(RpcError.InvalidParams("cursor must be a string"))
end ClientRequest

/** The 2026 per-request `_meta`: every stateless request says which revision it speaks. */
final case class RequestMeta(
    protocolVersion: Option[ProtocolVersion],
    clientInfo: Option[Json],
    clientCapabilities: Option[Json],
)

object RequestMeta:
  val VersionKey    = "io.modelcontextprotocol/protocolVersion"
  val ClientInfoKey = "io.modelcontextprotocol/clientInfo"
  val ClientCapsKey = "io.modelcontextprotocol/clientCapabilities"
  val ServerInfoKey = "io.modelcontextprotocol/serverInfo"

  def of(params: Json.Obj): RequestMeta =
    val meta = params.get("_meta") match
      case Some(o: Json.Obj) => o
      case _                 => Json.Obj()
    RequestMeta(
      meta.get(VersionKey).collect { case Json.Str(v) => ProtocolVersion(v) },
      meta.get(ClientInfoKey),
      meta.get(ClientCapsKey),
    )
end RequestMeta
