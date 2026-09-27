package heddle.mcp.protocol

import zio.json.{JsonDecoder, JsonEncoder}
import zio.json.ast.Json

/** A JSON-RPC request id: a number or a string, never null. */
enum RequestId:
  case Num(value: Long)
  case Str(value: String)

  def json: Json =
    this match
      case Num(n) => Json.Num(n)
      case Str(s) => Json.Str(s)

object RequestId:
  def fromJson(json: Json): Option[RequestId] =
    json match
      case Json.Str(s)                                                    => Some(Str(s))
      case Json.Num(n) if n.scale <= 0 || n.stripTrailingZeros.scale <= 0 =>
        scala.util.Try(n.longValueExact).toOption.map(Num(_))
      case _ => None

  given JsonEncoder[RequestId] = Json.encoder.contramap(_.json)
  given JsonDecoder[RequestId] = Json.decoder.mapOrFail(j => fromJson(j).toRight(s"not a request id: $j"))
end RequestId
