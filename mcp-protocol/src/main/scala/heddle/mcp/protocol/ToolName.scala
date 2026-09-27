package heddle.mcp.protocol

import scala.quoted.*
import zio.json.JsonCodec

/** A tool name: 1 to 128 of `A-Z a-z 0-9 _ - .` (SEP-986). Case-sensitive. */
opaque type ToolName = String

object ToolName:
  def from(raw: String): Either[ToolNameError, ToolName] =
    if raw.isEmpty then Left(ToolNameError.Empty)
    else if raw.length > 128 then Left(ToolNameError.TooLong(raw.length))
    else
      raw.indexWhere(c => !allowed(c)) match
        case -1 => Right(raw)
        case at => Left(ToolNameError.BadCharacter(raw(at), at))

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ToolName = ${ literal('raw) }

  private def allowed(c: Char): Boolean = c.isLetterOrDigit && c < 128 || c == '_' || c == '-' || c == '.'

  extension (n: ToolName) def value: String = n

  given JsonCodec[ToolName] = JsonCodec.string.transformOrFail(from(_).left.map(_.message), identity)

  private def literal(raw: Expr[String])(using Quotes): Expr[ToolName] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("ToolName(...) takes a literal; use ToolName.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not a tool name: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end ToolName
