package heddle.http.header

import java.time.{Instant, ZoneOffset}

/** `strftime` follows `LC_TIME`, so the names are fixed tables. The numbers come from the UTC offset, not tzdb. */
private[heddle] object DateStampPlatform extends DateStamp:
  // DayOfWeek.ordinal is Monday = 0. Month.ordinal is January = 0.
  private val Day   = Array("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  private val Month = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  def render(instant: Instant): String =
    val utc   = instant.atOffset(ZoneOffset.UTC)
    val dow   = Day(utc.getDayOfWeek.ordinal)
    val mon   = Month(utc.getMonth.ordinal)
    val day   = f"${utc.getDayOfMonth}%02d"
    val year  = f"${utc.getYear}%04d"
    val clock = f"${utc.getHour}%02d:${utc.getMinute}%02d:${utc.getSecond}%02d"
    s"$dow, $day $mon $year $clock GMT"
end DateStampPlatform
