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
  case SubscribeResource(uri: String)
  case UnsubscribeResource(uri: String)
  case Listen(operation: ToolName, arguments: Json.Obj)
  case ListEvents(cursor: Option[String])
  case PollEvents(
      name: EventName,
      arguments: Json.Obj,
      cursor: Option[String],
      maxAgeMs: Option[Long],
      maxEvents: Option[Int],
  )
  case SubscribeEvents(
      name: EventName,
      arguments: Json.Obj,
      url: String,
      secret: String,
      cursor: Option[String],
      maxAgeMs: Option[Long],
      ttl: TtlRequest,
  )
  case UnsubscribeEvents(name: EventName, arguments: Json.Obj, url: String)
  case StreamEvents(
      name: EventName,
      arguments: Json.Obj,
      cursor: Option[String],
      maxAgeMs: Option[Long],
      heartbeatMs: Option[Long],
  )

  def method: String =
    this match
      case Ping                                 => Methods.Ping
      case Discover                             => Methods.Discover
      case Initialize(_, _, _)                  => Methods.Initialize
      case ListTools(_)                         => Methods.ListTools
      case CallTool(_, _)                       => Methods.CallTool
      case ListResources(_)                     => Methods.ListResources
      case ListResourceTemplates(_)             => Methods.ListResourceTemplates
      case ReadResource(_)                      => Methods.ReadResource
      case SubscribeResource(_)                 => Methods.SubscribeResource
      case UnsubscribeResource(_)               => Methods.UnsubscribeResource
      case Listen(_, _)                         => Methods.Listen
      case ListEvents(_)                        => Methods.ListEvents
      case PollEvents(_, _, _, _, _)            => Methods.PollEvents
      case SubscribeEvents(_, _, _, _, _, _, _) => Methods.SubscribeEvents
      case UnsubscribeEvents(_, _, _)           => Methods.UnsubscribeEvents
      case StreamEvents(_, _, _, _, _)          => Methods.StreamEvents

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
      case SubscribeResource(uri)   => Json.Obj("uri" -> Json.Str(uri))
      case UnsubscribeResource(uri) => Json.Obj("uri" -> Json.Str(uri))
      case Listen(operation, args)  => Json.Obj("operation" -> Json.Str(operation.value), "arguments" -> args)
      case ListEvents(c)            => cursor(c)
      case PollEvents(name, args, c, age, max) =>
        named(name, args, Chunk.fromIterable(c.map("cursor" -> Json.Str(_))) ++ ageField(age) ++ maxField(max))
      case SubscribeEvents(name, args, url, secret, c, age, ttl) =>
        named(
          name,
          args,
          Chunk(
            "delivery" -> Json.Obj(
              "mode"   -> Json.Str("webhook"),
              "url"    -> Json.Str(url),
              "secret" -> Json.Str(secret),
            )
          ) ++ Chunk.fromIterable(c.map("cursor" -> Json.Str(_))) ++ ageField(age) ++ ttlField(ttl),
        )
      case UnsubscribeEvents(name, args, url) =>
        named(name, args, Chunk("delivery" -> Json.Obj("url" -> Json.Str(url))))
      case StreamEvents(name, args, c, age, heartbeat) =>
        named(
          name,
          args,
          Chunk.fromIterable(c.map("cursor" -> Json.Str(_))) ++ ageField(age) ++
            Chunk.fromIterable(heartbeat.map(ms => "heartbeatMs" -> Json.Num(ms))),
        )

  private def named(name: EventName, args: Json.Obj, extra: Chunk[(String, Json)]): Json.Obj =
    Json.Obj(Chunk("name" -> Json.Str(name.value), "arguments" -> args) ++ extra)

  private def ageField(age: Option[Long]): Chunk[(String, Json)] =
    Chunk.fromIterable(age.map(ms => "maxAgeMs" -> Json.Num(ms)))

  private def maxField(max: Option[Int]): Chunk[(String, Json)] =
    Chunk.fromIterable(max.map(n => "maxEvents" -> Json.Num(n)))

  private def ttlField(ttl: TtlRequest): Chunk[(String, Json)] =
    ttl match
      case TtlRequest.Absent     => Chunk.empty
      case TtlRequest.Unbounded  => Chunk("ttlMs" -> Json.Null)
      case TtlRequest.Millis(ms) => Chunk("ttlMs" -> Json.Num(ms))
end ClientRequest

/** Wire notification names. */
object Notifications:
  val Initialized               = "notifications/initialized"
  val Cancelled                 = "notifications/cancelled"
  val ResourceUpdated           = "notifications/resources/updated"
  val ToolsListChanged          = "notifications/tools/list_changed"
  val ResourcesListChanged      = "notifications/resources/list_changed"
  val SubscriptionsAcknowledged = "notifications/subscriptions/acknowledged"
  val OperationEvent            = "notifications/operation/event"
  val OperationError            = "notifications/operation/error"

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
  val SubscribeResource     = "resources/subscribe"
  val UnsubscribeResource   = "resources/unsubscribe"
  val Listen                = "subscriptions/listen"
  val ListEvents            = "events/list"
  val PollEvents            = "events/poll"
  val SubscribeEvents       = "events/subscribe"
  val UnsubscribeEvents     = "events/unsubscribe"
  val StreamEvents          = "events/stream"

  /** Methods whose `params.name` must be repeated in `Mcp-Name`. `events/list` has no name. */
  val named: Set[String] = Set(CallTool, PollEvents, SubscribeEvents, UnsubscribeEvents, StreamEvents)

  val requests: Set[String] =
    Set(
      Ping,
      Discover,
      Initialize,
      ListTools,
      CallTool,
      ListResources,
      ListResourceTemplates,
      ReadResource,
      SubscribeResource,
      UnsubscribeResource,
      Listen,
      ListEvents,
      PollEvents,
      SubscribeEvents,
      UnsubscribeEvents,
      StreamEvents,
    )
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
      case Methods.ReadResource          => uri(params, "resources/read").map(ReadResource(_))
      case Methods.SubscribeResource     => uri(params, "resources/subscribe").map(SubscribeResource(_))
      case Methods.UnsubscribeResource   => uri(params, "resources/unsubscribe").map(UnsubscribeResource(_))
      case Methods.Listen                => listen(params)
      case Methods.ListEvents            => optionalCursor(params).map(ListEvents(_))
      case Methods.PollEvents            => poll(params)
      case Methods.SubscribeEvents       => subscribe(params)
      case Methods.UnsubscribeEvents     => unsubscribe(params)
      case Methods.StreamEvents          => stream(params)
      case other                         => Left(RpcError.methodNotFound(other))

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

  private def listen(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name <- p.get("operation") match
        case Some(Json.Str(n)) => ToolName.from(n).left.map(e => RpcError.InvalidParams(e.message))
        case _                 => Left(RpcError.InvalidParams("missing operation"))
      args <- p.get("arguments") match
        case None              => Right(Json.Obj())
        case Some(o: Json.Obj) => Right(o)
        case Some(_)           => Left(RpcError.InvalidParams("arguments must be a JSON object"))
    yield Listen(name, args)

  private def uri(p: Json.Obj, method: String): Either[RpcError, String] =
    p.get("uri") match
      case Some(Json.Str(u)) => Right(u)
      case _                 => Left(RpcError.InvalidParams(s"$method needs a string uri"))

  private def cursor(p: Json.Obj): Either[RpcError, Option[String]] =
    p.get("cursor") match
      case None              => Right(None)
      case Some(Json.Str(c)) => Right(Some(c))
      case Some(_)           => Left(RpcError.InvalidParams("cursor must be a string"))

  /** `null` and a missing cursor are both "absent". Tool cursors stay strict; event cursors follow the events spec. */
  private def optionalCursor(p: Json.Obj): Either[RpcError, Option[String]] =
    p.get("cursor") match
      case None | Some(Json.Null) => Right(None)
      case Some(Json.Str(c))      => Right(Some(c))
      case Some(_)                => Left(RpcError.InvalidParams("cursor must be a string"))

  private def poll(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name <- eventName(p)
      args <- arguments(p)
      c    <- optionalCursor(p)
      age  <- optionalLong(p, "maxAgeMs")
      max  <- optionalInt(p, "maxEvents")
    yield PollEvents(name, args, c, age, max)

  private def subscribe(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name     <- eventName(p)
      args     <- arguments(p)
      delivery <- delivery(p)
      _        <- webhookMode(delivery)
      url      <- stringField(delivery, "url", "delivery.url")
      secret   <- stringField(delivery, "secret", "delivery.secret")
      c        <- optionalCursor(p)
      age      <- optionalLong(p, "maxAgeMs")
      ttl      <- ttl(p)
    yield SubscribeEvents(name, args, url, secret, c, age, ttl)

  private def unsubscribe(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name     <- eventName(p)
      args     <- arguments(p)
      delivery <- delivery(p)
      _        <- optionalWebhook(delivery)
      url      <- stringField(delivery, "url", "delivery.url")
    yield UnsubscribeEvents(name, args, url)

  private def stream(p: Json.Obj): Either[RpcError, ClientRequest] =
    for
      name <- eventName(p)
      args <- arguments(p)
      c    <- optionalCursor(p)
      age  <- optionalLong(p, "maxAgeMs")
      beat <- optionalLong(p, "heartbeatMs")
    yield StreamEvents(name, args, c, age, beat)

  private def eventName(p: Json.Obj): Either[RpcError, EventName] =
    p.get("name") match
      case Some(Json.Str(n)) => EventName.from(n).left.map(e => RpcError.InvalidParams(e.message))
      case _                 => Left(RpcError.InvalidParams("missing event name"))

  private def arguments(p: Json.Obj): Either[RpcError, Json.Obj] =
    p.get("arguments") match
      case None              => Right(Json.Obj())
      case Some(o: Json.Obj) => Right(o)
      case Some(_)           => Left(RpcError.InvalidParams("arguments must be a JSON object"))

  private def delivery(p: Json.Obj): Either[RpcError, Json.Obj] =
    p.get("delivery") match
      case Some(o: Json.Obj) => Right(o)
      case _                 => Left(RpcError.InvalidParams("delivery must be an object"))

  /** Subscribe names the mode. Push is a known mode this server answers as `Unsupported`, not a parse failure. */
  private def webhookMode(delivery: Json.Obj): Either[RpcError, Unit] =
    delivery.get("mode") match
      case Some(Json.Str("webhook")) => Right(())
      case Some(Json.Str("push"))    =>
        Left(RpcError.Unsupported(Json.Obj("feature" -> Json.Str("deliveryMode"), "value" -> Json.Str("push"))))
      case Some(Json.Str(_)) => Left(RpcError.InvalidParams("delivery.mode must be webhook"))
      case _                 => Left(RpcError.InvalidParams("delivery.mode must be webhook"))

  private def optionalWebhook(delivery: Json.Obj): Either[RpcError, Unit] =
    delivery.get("mode") match
      case None | Some(Json.Str("webhook")) => Right(())
      case Some(Json.Str("push"))           =>
        Left(RpcError.Unsupported(Json.Obj("feature" -> Json.Str("deliveryMode"), "value" -> Json.Str("push"))))
      case _ => Left(RpcError.InvalidParams("delivery.mode must be webhook"))

  private def stringField(obj: Json.Obj, key: String, label: String): Either[RpcError, String] =
    obj.get(key) match
      case Some(Json.Str(s)) => Right(s)
      case _                 => Left(RpcError.InvalidParams(s"$label must be a string"))

  private def ttl(p: Json.Obj): Either[RpcError, TtlRequest] =
    p.get("ttlMs") match
      case None            => Right(TtlRequest.Absent)
      case Some(Json.Null) => Right(TtlRequest.Unbounded)
      case Some(json)      => whole(json, "ttlMs").map(TtlRequest.Millis(_))

  private def optionalLong(p: Json.Obj, key: String): Either[RpcError, Option[Long]] =
    p.get(key) match
      case None       => Right(None)
      case Some(json) => whole(json, key).map(Some(_))

  private def optionalInt(p: Json.Obj, key: String): Either[RpcError, Option[Int]] =
    optionalLong(p, key).flatMap {
      case None                         => Right(None)
      case Some(n) if n <= Int.MaxValue => Right(Some(n.toInt))
      case Some(_)                      => Left(RpcError.InvalidParams(s"$key must be an integer"))
    }

  private def whole(json: Json, key: String): Either[RpcError, Long] =
    json match
      case Json.Num(n) if n.signum >= 0 =>
        scala.util.Try(n.longValueExact).toOption match
          case Some(v) => Right(v)
          case None    => Left(RpcError.InvalidParams(s"$key must be an integer"))
      case Json.Num(_) => Left(RpcError.InvalidParams(s"$key must be a non-negative integer"))
      case _           => Left(RpcError.InvalidParams(s"$key must be an integer"))
end ClientRequest

/** The 2026 per-request `_meta`: every stateless request says which revision it speaks. */
final case class RequestMeta(
    protocolVersion: Option[ProtocolVersion],
    clientInfo: Option[Json],
    clientCapabilities: Option[Json],
)

object RequestMeta:
  val VersionKey        = "io.modelcontextprotocol/protocolVersion"
  val ClientInfoKey     = "io.modelcontextprotocol/clientInfo"
  val ClientCapsKey     = "io.modelcontextprotocol/clientCapabilities"
  val ServerInfoKey     = "io.modelcontextprotocol/serverInfo"
  val SubscriptionIdKey = "io.modelcontextprotocol/subscriptionId"

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
