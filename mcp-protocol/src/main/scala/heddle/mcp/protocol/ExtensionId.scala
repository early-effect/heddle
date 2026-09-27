package heddle.mcp.protocol

import scala.quoted.*

/** An MCP extension identifier: `vendor-prefix/name`, as in `io.modelcontextprotocol/ui` (SEP-1724). */
opaque type ExtensionId = String

object ExtensionId:
  /** MCP Apps (SEP-1865). */
  val Ui: ExtensionId = "io.modelcontextprotocol/ui"

  def from(raw: String): Either[ExtensionIdError, ExtensionId] =
    val slash = raw.indexOf('/')
    if slash <= 0 || slash == raw.length - 1 || raw.indexOf('/', slash + 1) != -1 then
      Left(ExtensionIdError.NotPrefixSlashName)
    else
      raw.indexWhere(c => c != '/' && !allowed(c)) match
        case -1 => Right(raw)
        case at => Left(ExtensionIdError.BadCharacter(raw(at), at))

  private def allowed(c: Char): Boolean = c.isLetterOrDigit && c < 128 || "-._".contains(c)

  inline def apply(inline raw: String): ExtensionId = ${ literal('raw) }

  extension (id: ExtensionId) def value: String = id

  private def literal(raw: Expr[String])(using Quotes): Expr[ExtensionId] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("ExtensionId(...) takes a literal; use ExtensionId.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not an extension id: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end ExtensionId
