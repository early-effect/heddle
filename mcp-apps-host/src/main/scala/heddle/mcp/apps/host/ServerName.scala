package heddle.mcp.apps.host

import heddle.error.HeddleError
import scala.quoted.*

/** Why text is not a name a host can give a connected server. */
enum ServerNameError(val message: String) extends HeddleError:
  case Empty                extends ServerNameError("a server name is not empty")
  case TooLong(length: Int) extends ServerNameError(s"a server name is at most 64 characters, not $length")
  case BadCharacter(char: Char, at: Int)
      extends ServerNameError(s"'$char' at $at: a server name is letters, digits, '_', and '-'")

/** The host's own name for one connected MCP server (`counter`, `box-office`). Hosts build model-facing tool names from
  * it (`mcp__counter__inc`), so it holds only characters that are safe there.
  */
opaque type ServerName = String

object ServerName:
  def from(raw: String): Either[ServerNameError, ServerName] =
    if raw.isEmpty then Left(ServerNameError.Empty)
    else if raw.length > 64 then Left(ServerNameError.TooLong(raw.length))
    else
      raw.indexWhere(c => !(c < 128 && (c.isLetterOrDigit || c == '_' || c == '-'))) match
        case -1 => Right(raw)
        case at => Left(ServerNameError.BadCharacter(raw(at), at))

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ServerName = ${ literal('raw) }

  extension (n: ServerName) def value: String = n

  private def literal(raw: Expr[String])(using Quotes): Expr[ServerName] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("ServerName(...) takes a literal; use ServerName.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not a server name: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end ServerName
