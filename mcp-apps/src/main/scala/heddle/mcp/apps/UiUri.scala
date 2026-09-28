package heddle.mcp.apps

import scala.quoted.*
import zio.json.JsonCodec

/** Where an MCP App's view lives: a `ui://` resource URI (`ui://box-office/seats`). */
opaque type UiUri = String

object UiUri:
  val Scheme = "ui://"

  def from(raw: String): Either[UiUriError, UiUri] =
    if !raw.startsWith(Scheme) then Left(UiUriError.NotUi)
    else if raw.length == Scheme.length then Left(UiUriError.Empty)
    else
      raw.indexWhere(c => c.isWhitespace || c == '#') match
        case -1 => Right(raw)
        case at => Left(UiUriError.BadCharacter(raw(at), at))

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): UiUri = ${ literal('raw) }

  /** A JSON string, decoded strictly. */
  given JsonCodec[UiUri] = JsonCodec.string.transformOrFail(from(_).left.map(_.message), identity)

  extension (u: UiUri) def value: String = u

  private def literal(raw: Expr[String])(using Quotes): Expr[UiUri] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("UiUri(...) takes a literal; use UiUri.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not a ui:// resource uri: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end UiUri
