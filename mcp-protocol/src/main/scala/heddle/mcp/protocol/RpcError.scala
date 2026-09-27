package heddle.mcp.protocol

import zio.Chunk
import zio.json.ast.Json

/** A JSON-RPC error object. Standard codes are cases carrying the wire message; any other code is `Other`. */
enum RpcError(val code: Int):
  case ParseError(message: String)                                              extends RpcError(-32700)
  case InvalidRequest(message: String)                                          extends RpcError(-32600)
  case MethodNotFound(message: String)                                          extends RpcError(-32601)
  case InvalidParams(message: String)                                           extends RpcError(-32602)
  case Internal(message: String)                                                extends RpcError(-32603)
  case HeaderMismatch(message: String)                                          extends RpcError(-32020)
  case UnsupportedVersion(requested: String, supported: Chunk[ProtocolVersion]) extends RpcError(-32022)
  case Other(override val code: Int, message: String, data: Option[Json])       extends RpcError(code)

  def text: String =
    this match
      case ParseError(m)            => m
      case InvalidRequest(m)        => m
      case MethodNotFound(m)        => m
      case InvalidParams(m)         => m
      case Internal(m)              => m
      case HeaderMismatch(m)        => m
      case UnsupportedVersion(_, _) => RpcError.UnsupportedText
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
      case Other(_, _, d) => d
      case _              => None
    Json.Obj(Chunk("code" -> Json.Num(code), "message" -> Json.Str(text)) ++ Chunk.fromIterable(data.map("data" -> _)))
  end json
end RpcError

object RpcError:
  private val UnsupportedText = "Unsupported protocol version"

  def methodNotFound(method: String): RpcError = MethodNotFound(s"Method not found: $method")

  /** Reads an error object back into its case. Total: anything with an integer code and a message is an `RpcError`. */
  def fromJson(obj: Json.Obj): Either[String, RpcError] =
    (obj.get("code").flatMap(integer), obj.get("message")) match
      case (Some(code), Some(Json.Str(m))) => Right(byCode(code, m, obj.get("data")))
      case _                               => Left("error must have an integer code and a string message")

  private def byCode(code: Int, message: String, data: Option[Json]): RpcError =
    (code, data) match
      case (-32700, None)                                            => ParseError(message)
      case (-32600, None)                                            => InvalidRequest(message)
      case (-32601, None)                                            => MethodNotFound(message)
      case (-32602, None)                                            => InvalidParams(message)
      case (-32603, None)                                            => Internal(message)
      case (-32020, None)                                            => HeaderMismatch(message)
      case (-32022, Some(d: Json.Obj)) if message == UnsupportedText =>
        unsupported(d).getOrElse(Other(code, message, data))
      case _ => Other(code, message, data)

  private def unsupported(d: Json.Obj): Option[RpcError] =
    for
      requested <- d.get("requested").collect { case Json.Str(s) => s }
      supported <- d.get("supported").collect { case Json.Arr(vs) =>
        Chunk.fromIterable(vs.collect { case Json.Str(v) => ProtocolVersion(v) })
      }
    yield UnsupportedVersion(requested, supported)

  private def integer(json: Json): Option[Int] =
    json match
      case Json.Num(n) => scala.util.Try(n.intValueExact).toOption
      case _           => None
end RpcError
