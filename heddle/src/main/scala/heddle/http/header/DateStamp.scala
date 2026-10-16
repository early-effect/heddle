package heddle.http.header

import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.Chunk

/** IMF-fixdate for one instant. The names are part of the wire grammar (RFC 9110 §5.6.7), so each platform supplies
  * them without a locale database. [[stamp]] is what the origin `Date` ref stores.
  */
private[heddle] trait DateStamp:
  def render(instant: Instant): String

  final def stamp(instant: Instant): OriginDate.Stamp =
    val value = render(instant)
    OriginDate.Stamp(value, Chunk.fromArray(s"Date: $value\r\n".getBytes(StandardCharsets.US_ASCII)))
