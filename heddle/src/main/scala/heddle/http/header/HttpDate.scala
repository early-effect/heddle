package heddle.http.header

import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.format.DateTimeFormatter

/** HTTP dates. Parsing is RFC 6265 §5.1.1, which reads all three RFC 9110 §5.6.7 forms (IMF-fixdate, RFC 850, asctime)
  * and the other shapes cookies arrive in; it is total and never throws.
  */
private[heddle] object HttpDate:
  def parse(raw: String): Option[Instant] =
    val tokens = raw.split("[\\t !-/;-@\\[-`{-~]+").toList.filter(_.nonEmpty)
    val found  = tokens.foldLeft(Found()) { (f, token) =>
      if f.time.isEmpty && time(token).isDefined then f.copy(time = time(token))
      else if f.day.isEmpty && digits(token, 1, 2).isDefined then f.copy(day = digits(token, 1, 2))
      else if f.month.isEmpty && month(token).isDefined then f.copy(month = month(token))
      else if f.year.isEmpty && digits(token, 2, 4).isDefined then f.copy(year = digits(token, 2, 4))
      else f
    }
    for
      (h, m, s) <- found.time.filter((h, m, s) => h <= 23 && m <= 59 && s <= 59)
      mon       <- found.month
      y0        <- found.year
      year = if y0 >= 70 && y0 <= 99 then y0 + 1900 else if y0 <= 69 then y0 + 2000 else y0
      day <- found.day.filter(_ >= 1)
      if year >= 1601 && day <= LocalDate.of(year, mon, 1).lengthOfMonth
    yield LocalDate.of(year, mon, day).atTime(h, m, s).toInstant(ZoneOffset.UTC)
  end parse

  def render(instant: Instant): String =
    DateTimeFormatter.RFC_1123_DATE_TIME.format(instant.atOffset(ZoneOffset.UTC))

  private final case class Found(
      time: Option[(Int, Int, Int)] = None,
      day: Option[Int] = None,
      month: Option[Int] = None,
      year: Option[Int] = None,
  )

  private val Months = List("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

  /** `1*2DIGIT ":" 1*2DIGIT ":" 1*2DIGIT`, then anything that is not a digit. */
  private def time(token: String): Option[(Int, Int, Int)] =
    token.split(":", -1) match
      case Array(h, m, s) =>
        for
          hh <- digits(h, 1, 2)
          mm <- digits(m, 1, 2)
          ss <- digits(s.takeWhile(_.isDigit), 1, 2).filter(_ =>
            s.drop(s.takeWhile(_.isDigit).length).forall(!_.isDigit)
          )
        yield (hh, mm, ss)
      case _ => None

  /** `min` to `max` ASCII digits at the start, then nothing that is a digit. */
  private def digits(token: String, min: Int, max: Int): Option[Int] =
    val lead = token.takeWhile(c => c >= '0' && c <= '9')
    Option.when(
      lead.length >= min && lead.length <= max && token.drop(lead.length).forall(c => !(c >= '0' && c <= '9'))
    )(
      lead.toInt
    )

  private def month(token: String): Option[Int] =
    Months.indexOf(token.take(3).toLowerCase) match
      case -1 => None
      case i  => Some(i + 1)
end HttpDate
