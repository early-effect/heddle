package heddle.mcp.protocol

import zio.Chunk
import zio.json.ast.Json

/** A JSON-RPC error object. Standard codes are cases carrying the wire message; any other code is `Other`.
  *
  * `-32023` is `OperationFailed` when the message is `operation failed`, which this library already published for a
  * stream gate. `NotFound` is the same code with the message `NotFound`. The two are not interchangeable.
  */
enum RpcError(val code: Int):
  case ParseError(message: String)                                              extends RpcError(-32700)
  case InvalidRequest(message: String)                                          extends RpcError(-32600)
  case MethodNotFound(message: String)                                          extends RpcError(-32601)
  case InvalidParams(message: String)                                           extends RpcError(-32602)
  case Internal(message: String)                                                extends RpcError(-32603)
  case HeaderMismatch(message: String)                                          extends RpcError(-32020)
  case ResourceNotFound(message: String)                                        extends RpcError(-32002)
  case UnsupportedVersion(requested: String, supported: Chunk[ProtocolVersion]) extends RpcError(-32022)

  /** A subscription's gate refused. `data` is the operation error's JSON, when it has one. */
  case OperationFailed(data: Option[Json]) extends RpcError(-32023)

  /** No such event type or subscription. `kind` is `event` or `subscription` when the server names it. */
  case NotFound(kind: Option[String]) extends RpcError(-32023)

  /** The principal may not use this event, or no principal was presented for a webhook. */
  case Forbidden(reason: Option[String]) extends RpcError(-32024)

  /** A server limit. `limit` names it (`subscriptions`, `verification`); `max` is the ceiling when there is one. */
  case ResourceExhausted(limit: String, max: Option[Int]) extends RpcError(-32025)

  /** The request is well formed and this server will not do it. `data` says which feature. */
  case Unsupported(data: Json.Obj) extends RpcError(-32026)

  /** A webhook callback failed verification or could not be reached. `reason` is a category, never a response body. */
  case CallbackEndpointError(reason: CallbackFailure) extends RpcError(-32027)

  case Other(override val code: Int, message: String, data: Option[Json]) extends RpcError(code)

  def text: String =
    this match
      case ParseError(m)            => m
      case InvalidRequest(m)        => m
      case MethodNotFound(m)        => m
      case InvalidParams(m)         => m
      case Internal(m)              => m
      case HeaderMismatch(m)        => m
      case ResourceNotFound(m)      => m
      case UnsupportedVersion(_, _) => RpcError.UnsupportedText
      case OperationFailed(_)       => RpcError.OperationText
      case NotFound(_)              => RpcError.NotFoundText
      case Forbidden(_)             => RpcError.ForbiddenText
      case ResourceExhausted(_, _)  => RpcError.ExhaustedText
      case Unsupported(_)           => RpcError.UnsupportedFeatureText
      case CallbackEndpointError(_) => RpcError.CallbackText
      case Other(_, m, _)           => m

  def json: Json.Obj =
    val data = this match
      case UnsupportedVersion(requested, supported) =>
        Some(
          Json.Obj(
            "supported" -> Json.Arr(supported.map(v => Json.Str(v.value))),
            "requested" -> Json.Str(requested),
          )
        )
      case OperationFailed(d)            => d
      case NotFound(kind)                => kind.map(k => Json.Obj("kind" -> Json.Str(k)))
      case Forbidden(reason)             => reason.map(r => Json.Obj("reason" -> Json.Str(r)))
      case ResourceExhausted(limit, max) =>
        Some(Json.Obj(Chunk("limit" -> Json.Str(limit)) ++ Chunk.fromIterable(max.map(n => "max" -> Json.Num(n)))))
      case Unsupported(d)                => Some(d)
      case CallbackEndpointError(reason) => Some(Json.Obj("reason" -> Json.Str(reason.wire)))
      case Other(_, _, d)                => d
      case _                             => None
    Json.Obj(Chunk("code" -> Json.Num(code), "message" -> Json.Str(text)) ++ Chunk.fromIterable(data.map("data" -> _)))
end RpcError

object RpcError:
  private val UnsupportedText        = "Unsupported protocol version"
  private val OperationText          = "operation failed"
  private val NotFoundText           = "NotFound"
  private val ForbiddenText          = "Forbidden"
  private val ExhaustedText          = "ResourceExhausted"
  private val UnsupportedFeatureText = "Unsupported"
  private val CallbackText           = "CallbackEndpointError"

  def methodNotFound(method: String): RpcError = MethodNotFound(s"Method not found: $method")

  /** Reads an error object back into its case. Anything with an integer code and a string message is an `RpcError`. */
  def fromJson(obj: Json.Obj): Option[RpcError] =
    (obj.get("code").flatMap(integer), obj.get("message")) match
      case (Some(code), Some(Json.Str(m))) => Some(byCode(code, m, obj.get("data")))
      case _                               => None

  private def byCode(code: Int, message: String, data: Option[Json]): RpcError =
    (code, data) match
      case (-32700, None)                                            => ParseError(message)
      case (-32600, None)                                            => InvalidRequest(message)
      case (-32601, None)                                            => MethodNotFound(message)
      case (-32602, None)                                            => InvalidParams(message)
      case (-32603, None)                                            => Internal(message)
      case (-32020, None)                                            => HeaderMismatch(message)
      case (-32002, None)                                            => ResourceNotFound(message)
      case (-32022, Some(d: Json.Obj)) if message == UnsupportedText =>
        unsupported(d).getOrElse(Other(code, message, data))
      case (-32023, _) if message == OperationText => OperationFailed(data)
      case (-32023, _) if message == NotFoundText  => NotFound(kindOf(data))
      case (-32024, _) if message == ForbiddenText => Forbidden(stringField(data, "reason"))
      case (-32025, _) if message == ExhaustedText =>
        exhausted(data).getOrElse(Other(code, message, data))
      case (-32026, Some(d: Json.Obj)) if message == UnsupportedFeatureText => Unsupported(d)
      case (-32027, _) if message == CallbackText                           =>
        callback(data).getOrElse(Other(code, message, data))
      case _ => Other(code, message, data)

  private def unsupported(d: Json.Obj): Option[RpcError] =
    for
      requested <- d.get("requested").collect { case Json.Str(s) => s }
      supported <- d.get("supported").collect { case Json.Arr(vs) =>
        Chunk.fromIterable(vs.collect { case Json.Str(v) => ProtocolVersion(v) })
      }
    yield UnsupportedVersion(requested, supported)

  private def kindOf(data: Option[Json]): Option[String] =
    data match
      case Some(obj: Json.Obj) =>
        obj.get("kind") match
          case Some(Json.Str(s)) => Some(s)
          case _                 => None
      case _ => None

  private def stringField(data: Option[Json], key: String): Option[String] =
    data match
      case Some(obj: Json.Obj) =>
        obj.get(key) match
          case Some(Json.Str(s)) => Some(s)
          case _                 => None
      case _ => None

  private def exhausted(data: Option[Json]): Option[RpcError] =
    data match
      case Some(obj: Json.Obj) =>
        obj.get("limit") match
          case Some(Json.Str(limit)) =>
            obj.get("max") match
              case None              => Some(ResourceExhausted(limit, None))
              case Some(Json.Num(n)) =>
                scala.util.Try(n.intValueExact).toOption.map(max => ResourceExhausted(limit, Some(max)))
              case Some(_) => None
          case _ => None
      case _ => None

  private def callback(data: Option[Json]): Option[RpcError] =
    stringField(data, "reason").flatMap(CallbackFailure.fromWire).map(CallbackEndpointError(_))

  private def integer(json: Json): Option[Int] =
    json match
      case Json.Num(n) => scala.util.Try(n.intValueExact).toOption
      case _           => None
end RpcError
