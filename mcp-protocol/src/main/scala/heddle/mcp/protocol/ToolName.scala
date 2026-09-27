package heddle.mcp.protocol

import scala.quoted.*
import zio.json.JsonCodec

/** A tool name: 1 to 128 of `A-Z a-z 0-9 _ - .` (SEP-986). Case-sensitive. */
opaque type ToolName = String

object ToolName:
  def from(raw: String): Either[String, ToolName] =
    if valid(raw) then Right(raw) else Left(s"not a tool name (1-128 of A-Z a-z 0-9 _ - .): $raw")

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ToolName = ${ literal('raw) }

  def valid(raw: String): Boolean =
    raw.nonEmpty && raw.length <= 128 && raw.forall(c =>
      c.isLetterOrDigit && c < 128 || c == '_' || c == '-' || c == '.'
    )

  extension (n: ToolName) def value: String = n

  given JsonCodec[ToolName] = JsonCodec.string.transformOrFail(from, identity)

  private def literal(raw: Expr[String])(using Quotes): Expr[ToolName] =
    import quotes.reflect.report
    raw.value match
      case None                 => report.errorAndAbort("ToolName(...) takes a literal; use ToolName.from")
      case Some(s) if !valid(s) => report.errorAndAbort(s"not a tool name (1-128 of A-Z a-z 0-9 _ - .): $s")
      case Some(s)              => Expr(s)
end ToolName
