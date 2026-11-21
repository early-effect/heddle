package heddle.mcp.protocol

import scala.quoted.*

/** An event name: 1 to 128 of `A-Z a-z 0-9 _ - .`. Same alphabet as a tool name, and not a tool name. */
opaque type EventName = String

object EventName:
  def from(raw: String): Either[EventNameError, EventName] =
    if raw.isEmpty then Left(EventNameError.Empty)
    else if raw.length > 128 then Left(EventNameError.TooLong(raw.length))
    else
      raw.indexWhere(c => !allowed(c)) match
        case -1 => Right(raw)
        case at => Left(EventNameError.BadCharacter(raw(at), at))

  /** A literal, checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): EventName = ${ literal('raw) }

  private def allowed(c: Char): Boolean = c.isLetterOrDigit && c < 128 || c == '_' || c == '-' || c == '.'

  extension (n: EventName) def value: String = n

  private def literal(raw: Expr[String])(using Quotes): Expr[EventName] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("EventName(...) takes a literal; use EventName.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not an event name: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end EventName

/** Why text is not an event name. */
enum EventNameError(val message: String):
  case Empty                extends EventNameError("an event name is 1 to 128 characters, and this is empty")
  case TooLong(length: Int) extends EventNameError(s"an event name is at most 128 characters, and this is $length")
  case BadCharacter(char: Char, at: Int) extends EventNameError(s"'$char' at $at is not one of A-Z a-z 0-9 _ - .")
