package heddle.sse

import scala.annotation.publicInBinary
import scala.quoted.*

/** An SSE `event` or `id` value. It holds no CR or LF, so it cannot end its field or inject another one. */
final case class SseField @publicInBinary private[sse] (value: String):
  override def toString: String = value

object SseField:
  def from(raw: String): Either[String, SseField] =
    if raw.exists(c => c == '\n' || c == '\r') then Left(s"SSE field must not contain CR or LF: ${raw.take(40)}")
    else Right(new SseField(raw))

  def of(n: Long): SseField = new SseField(n.toString)

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): SseField = ${ literal('raw) }

  private def literal(raw: Expr[String])(using Quotes): Expr[SseField] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("SseField(...) takes a literal; use SseField.from")
      case Some(s) if s.exists(c => c == '\n' || c == '\r') =>
        report.errorAndAbort("SSE field must not contain CR or LF")
      case Some(s) => '{ new SseField(${ Expr(s) }) }
end SseField
