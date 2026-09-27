package heddle.mcp.protocol

import zio.json.ast.Json

/** One JSON-RPC 2.0 message. `method` stays a string here; [[ClientRequest]] gives requests their types. */
enum Message:
  case Request(id: RequestId, method: String, params: Json.Obj)
  case Notification(method: String, params: Json.Obj)
  case Result(id: RequestId, result: Json.Obj)

  /** `id` is `None` only when the request could not be read far enough to find one. */
  case Error(id: Option[RequestId], error: RpcError)

  def json: Json.Obj =
    val version = "jsonrpc" -> Json.Str(Message.Version)
    this match
      case Request(id, m, p)  => Json.Obj(version, "id" -> id.json, "method" -> Json.Str(m), "params" -> p)
      case Notification(m, p) => Json.Obj(version, "method" -> Json.Str(m), "params" -> p)
      case Result(id, r)      => Json.Obj(version, "id" -> id.json, "result" -> r)
      case Error(id, e)       => Json.Obj(version, "id" -> id.fold[Json](Json.Null)(_.json), "error" -> e.json)
end Message

object Message:
  val Version = "2.0"

  /** Total. A value that is not a JSON-RPC message comes back as the `Error` a server should answer with. */
  def decode(json: Json): Either[Message.Error, Message] =
    json match
      case obj: Json.Obj => fromObj(obj)
      case _             => Left(new Message.Error(None, RpcError.InvalidRequest("Invalid Request: not an object")))

  private def fromObj(obj: Json.Obj): Either[Message.Error, Message] =
    val rawId                                                = obj.get("id")
    val id                                                   = rawId.flatMap(RequestId.fromJson)
    def invalid(why: String): Either[Message.Error, Message] =
      Left(new Message.Error(id, RpcError.InvalidRequest(s"Invalid Request: $why")))
    if !obj.get("jsonrpc").contains(Json.Str(Version)) then invalid("jsonrpc must be \"2.0\"")
    else
      (obj.get("method"), obj.get("result"), obj.get("error")) match
        case (Some(Json.Str(m)), None, None) =>
          params(obj.get("params")) match
            case None    => invalid("params must be an object")
            case Some(p) =>
              (rawId, id) match
                case (None, _)          => Right(Notification(m, p))
                case (Some(_), Some(i)) => Right(Request(i, m, p))
                case (Some(_), None)    => invalid("id must be a number or a string")
        case (None, Some(r: Json.Obj), None) =>
          id.fold(invalid("a result needs an id"))(i => Right(Result(i, r)))
        case (None, None, Some(e: Json.Obj)) =>
          RpcError
            .fromJson(e)
            .fold(invalid("an error needs an integer code and a string message"))(err => Right(Error(id, err)))
        case _ => invalid("exactly one of method, result, error")
    end if
  end fromObj

  private def params(raw: Option[Json]): Option[Json.Obj] =
    raw match
      case None              => Some(Json.Obj())
      case Some(o: Json.Obj) => Some(o)
      case Some(_)           => None

  /** A 2025-11-25 session request: params as the method defines them. */
  def request(id: RequestId, req: ClientRequest): Message.Request =
    new Message.Request(id, req.method, req.params)

  /** A 2026-07-28 request: the params carry `_meta` naming the revision and the client's capabilities. */
  def stateless(id: RequestId, req: ClientRequest, capabilities: Json.Obj = Json.Obj()): Message.Request =
    val meta = Json.Obj(
      RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> capabilities,
    )
    new Message.Request(id, req.method, Json.Obj(req.params.fields :+ ("_meta" -> meta)))

  /** The request for `era`: stateless requests carry `_meta`, session requests do not. */
  def in(era: Era, id: RequestId, req: ClientRequest, capabilities: Json.Obj = Json.Obj()): Message.Request =
    era match
      case Era.Stateless => stateless(id, req, capabilities)
      case Era.Session   => request(id, req)
end Message
