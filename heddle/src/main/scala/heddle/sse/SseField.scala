package heddle.sse

import scala.annotation.publicInBinary
import scala.quoted.*

/** An SSE `event` or `id` value. It holds no CR or LF, so it cannot end its field or inject another one. */
final case class SseField @publicInBinary private[sse] (value: String):
  override def toString: String = value

object SseField:
  def from(raw: String): Either[SseFieldError, SseField] =
    raw.indexWhere(c => c == '\n' || c == '\r') match
      case -1 => Right(new SseField(raw))
      case at => Left(SseFieldError.LineBreak(at))

  def of(n: Long): SseField = new SseField(n.toString)

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): SseField = ${ literal('raw) }

  private def literal(raw: Expr[String])(using Quotes): Expr[SseField] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("SseField(...) takes a literal; use SseField.from")
      case Some((_, Left(e)))  => report.errorAndAbort(e.message)
      case Some((s, Right(_))) => '{ new SseField(${ Expr(s) }) }
end SseField
