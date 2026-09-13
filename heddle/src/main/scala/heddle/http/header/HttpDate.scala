package heddle.http.header

import java.time.Instant
import java.time.format.DateTimeFormatter

private[heddle] object HttpDate:
  def parse(raw: String): Option[Instant] =
    try Some(Instant.from(DateTimeFormatter.RFC_1123_DATE_TIME.parse(raw.trim)))
    catch case _: Exception => None

  def render(instant: Instant): String =
    DateTimeFormatter.RFC_1123_DATE_TIME.format(instant.atOffset(java.time.ZoneOffset.UTC))
