package heddle.mcp.protocol

import zio.Chunk
import zio.json.ast.Json

/** Extra headers on a webhook POST. A heddle extension: the events spec has no `delivery.headers`.
  *
  * Values are secrets. `toString` prints names only. An error names a header and the rule it broke, and never the
  * value. The headers are not part of a subscription's identity: a later subscribe replaces them.
  *
  * At most [[DeliveryHeaders.MaxCount]] names, each name at most [[DeliveryHeaders.MaxName]] characters, each value at
  * most [[DeliveryHeaders.MaxValue]] characters, and the names and values together at most [[DeliveryHeaders.MaxTotal]]
  * characters. A name is an HTTP token. A value has no ASCII control character. Reserved names are refused
  * case-insensitively: `Host`, `Content-Length`, `Content-Type`, the hop-by-hop headers, `webhook-*`, and `X-MCP-*`.
  */
final class DeliveryHeaders private (private[heddle] val entries: Chunk[(String, String)]):
  def isEmpty: Boolean = entries.isEmpty

  /** The `delivery.headers` field, omitted when empty so a subscribe with no extra headers keeps the spec shape. */
  private[protocol] def wireField: Chunk[(String, Json)] =
    if entries.isEmpty then Chunk.empty
    else Chunk("headers" -> Json.Obj(entries.map((name, value) => name -> Json.Str(value))))

  override def toString: String =
    s"DeliveryHeaders(${entries.map(_._1).mkString(",")})"

  override def equals(other: Any): Boolean =
    other match
      case that: DeliveryHeaders => entries == that.entries
      case _                     => false

  override def hashCode: Int = entries.hashCode
end DeliveryHeaders

object DeliveryHeaders:
  val MaxCount: Int = 16
  val MaxName: Int  = 64
  val MaxValue: Int = 4096
  val MaxTotal: Int = 8192

  val empty: DeliveryHeaders = new DeliveryHeaders(Chunk.empty)

  def parse(json: Json): Either[RpcError, DeliveryHeaders] =
    json match
      case Json.Obj(fields) =>
        if fields.length > MaxCount then Left(RpcError.InvalidParams(countMessage(fields.length)))
        else collect(fields, Set.empty, Chunk.empty, 0)
      case _ => Left(RpcError.InvalidParams("delivery.headers must be an object"))

  private def collect(
      rest: Chunk[(String, Json)],
      seen: Set[String],
      acc: Chunk[(String, String)],
      total: Int,
  ): Either[RpcError, DeliveryHeaders] =
    rest.headOption match
      case None =>
        if total > MaxTotal then Left(RpcError.InvalidParams(s"delivery.headers exceeds $MaxTotal characters"))
        else Right(if acc.isEmpty then empty else new DeliveryHeaders(acc))
      case Some((name, value)) =>
        checked(name, value, seen, total) match
          case Left(error)                         => Left(error)
          case Right((nextSeen, entry, nextTotal)) =>
            collect(rest.drop(1), nextSeen, acc :+ entry, nextTotal)

  private def checked(
      name: String,
      value: Json,
      seen: Set[String],
      total: Int,
  ): Either[RpcError, (Set[String], (String, String), Int)] =
    if !token(name) then Left(RpcError.InvalidParams("delivery.headers has a name that is not an HTTP token"))
    else if name.length > MaxName then
      Left(RpcError.InvalidParams(s"delivery.headers has a name longer than $MaxName characters"))
    else if reserved(name) then Left(RpcError.InvalidParams(s"delivery.headers rejects $name"))
    else if seen.contains(name.toLowerCase) then Left(RpcError.InvalidParams(s"delivery.headers repeats $name"))
    else
      value match
        case Json.Str(text) =>
          if text.length > MaxValue then
            Left(RpcError.InvalidParams(s"delivery.headers.$name is longer than $MaxValue characters"))
          else if controlled(text) then
            Left(RpcError.InvalidParams(s"delivery.headers.$name must not contain a control character"))
          else Right((seen + name.toLowerCase, (name, text), total + name.length + text.length))
        case _ => Left(RpcError.InvalidParams(s"delivery.headers.$name must be a string"))

  /** RFC 9110 `tchar`, ASCII only. */
  private def token(name: String): Boolean =
    name.nonEmpty && name.forall(c =>
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || "!#$%&'*+-.^_`|~".contains(c)
    )

  private def controlled(value: String): Boolean =
    value.exists(c => c < ' ' || c == 0x7f)

  /** Hop-by-hop headers (RFC 9110), plus the fields this sender writes itself, the signing headers, and `X-MCP-*`. */
  private def reserved(name: String): Boolean =
    val lower = name.toLowerCase
    HopByHop.contains(lower) || lower.startsWith("webhook-") || lower.startsWith("x-mcp-")

  private val HopByHop: Set[String] = Set(
    "host",
    "content-length",
    "content-type",
    "connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "transfer-encoding",
    "upgrade",
  )

  private def countMessage(count: Int): String =
    s"delivery.headers has $count names and the cap is $MaxCount"
end DeliveryHeaders
