package heddle.http.header

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import java.util.Locale

/** The JDK already has English weekday and month names, so the origin stamp uses them. */
private[heddle] object DateStampPlatform extends DateStamp:
  private val ImfFixdate =
    DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'", Locale.US).withZone(ZoneOffset.UTC)

  def render(instant: Instant): String =
    ImfFixdate.format(instant)
