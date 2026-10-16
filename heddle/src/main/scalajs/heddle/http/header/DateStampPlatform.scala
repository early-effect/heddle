package heddle.http.header

import java.time.Instant
import scala.scalajs.js

/** ECMA-262 defines `Date.prototype.toUTCString` as the IMF-fixdate form, zero-padded day included. */
private[heddle] object DateStampPlatform extends DateStamp:
  def render(instant: Instant): String =
    new js.Date(instant.toEpochMilli.toDouble).toUTCString()
