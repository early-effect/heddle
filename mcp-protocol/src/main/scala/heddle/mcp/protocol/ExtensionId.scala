package heddle.mcp.protocol

import scala.quoted.*

/** An MCP extension identifier: `vendor-prefix/name`, as in `io.modelcontextprotocol/ui` (SEP-1724). */
opaque type ExtensionId = String

object ExtensionId:
  /** MCP Apps (SEP-1865). */
  val Ui: ExtensionId = "io.modelcontextprotocol/ui"

  def from(raw: String): Either[String, ExtensionId] =
    if valid(raw) then Right(raw) else Left(s"not an extension id (prefix/name): $raw")

  inline def apply(inline raw: String): ExtensionId = ${ literal('raw) }

  def valid(raw: String): Boolean =
    raw.split('/').toList match
      case prefix :: name :: Nil =>
        prefix.nonEmpty && name.nonEmpty && (prefix + name).forall(c =>
          c.isLetterOrDigit && c < 128 || "-._".contains(c)
        )
      case _ => false

  extension (id: ExtensionId) def value: String = id

  private def literal(raw: Expr[String])(using Quotes): Expr[ExtensionId] =
    import quotes.reflect.report
    raw.value match
      case None                 => report.errorAndAbort("ExtensionId(...) takes a literal; use ExtensionId.from")
      case Some(s) if !valid(s) => report.errorAndAbort(s"not an extension id (prefix/name): $s")
      case Some(s)              => Expr(s)
end ExtensionId
