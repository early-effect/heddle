package heddle.mcp.apps

import scala.quoted.*

/** Where an MCP App's view lives: a `ui://` resource URI (`ui://box-office/seats`). */
opaque type UiUri = String

object UiUri:
  val Scheme = "ui://"

  def from(raw: String): Either[String, UiUri] =
    if valid(raw) then Right(raw) else Left(s"not a ui:// resource uri: $raw")

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): UiUri = ${ literal('raw) }

  def valid(raw: String): Boolean =
    raw.startsWith(Scheme) && raw.length > Scheme.length && !raw.exists(c => c.isWhitespace || c == '#')

  extension (u: UiUri) def value: String = u

  private def literal(raw: Expr[String])(using Quotes): Expr[UiUri] =
    import quotes.reflect.report
    raw.value match
      case None                 => report.errorAndAbort("UiUri(...) takes a literal; use UiUri.from")
      case Some(s) if !valid(s) => report.errorAndAbort(s"not a ui:// resource uri: $s")
      case Some(s)              => Expr(s)
end UiUri
