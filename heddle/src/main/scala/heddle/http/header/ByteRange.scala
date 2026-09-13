package heddle.http.header

import zio.Chunk

enum ByteRange:
  case Inclusive(start: Long, end: Option[Long])
  case Suffix(length: Long)

final case class RangeSpec(unit: String, ranges: Chunk[ByteRange])

object RangeSpec:
  def parse(raw: String): Option[RangeSpec] =
    val s  = raw.trim
    val eq = s.indexOf('=')
    if eq <= 0 then None
    else
      val unit = s.substring(0, eq).trim
      val rest = s.substring(eq + 1).trim
      if rest.isEmpty then None
      else
        val parsed = rest.split(',').toList.map(_.trim).filter(_.nonEmpty).flatMap(parseOne)
        if parsed.isEmpty then None else Some(RangeSpec(unit, Chunk.fromIterable(parsed)))
  end parse

  private def parseOne(raw: String): Option[ByteRange] =
    if raw.startsWith("-") then raw.substring(1).toLongOption.filter(_ >= 0).map(ByteRange.Suffix.apply)
    else
      val dash = raw.indexOf('-')
      if dash < 0 then None
      else
        raw.substring(0, dash).toLongOption.filter(_ >= 0).flatMap { start =>
          val endRaw = raw.substring(dash + 1).trim
          if endRaw.isEmpty then Some(ByteRange.Inclusive(start, None))
          else endRaw.toLongOption.filter(_ >= start).map(end => ByteRange.Inclusive(start, Some(end)))
        }

  def render(spec: RangeSpec): String =
    val body = spec.ranges
      .map {
        case ByteRange.Inclusive(start, Some(end)) => s"$start-$end"
        case ByteRange.Inclusive(start, None)      => s"$start-"
        case ByteRange.Suffix(n)                   => s"-$n"
      }
      .mkString(", ")
    s"${spec.unit}=$body"
end RangeSpec

final case class ContentRange(unit: String, start: Option[Long], end: Option[Long], complete: Option[Long]):
  def render: String =
    val span =
      (start, end) match
        case (Some(s), Some(e)) => s"$s-$e"
        case _                  => "*"
    val total = complete.map(_.toString).getOrElse("*")
    s"$unit $span/$total"

object ContentRange:
  def satisfiable(unit: String, start: Long, end: Long, complete: Long): ContentRange =
    ContentRange(unit, Some(start), Some(end), Some(complete))

  def unsatisfiable(complete: Long, unit: String = "bytes"): ContentRange =
    ContentRange(unit, None, None, Some(complete))

final case class EntityTag(tag: String, weak: Boolean = false):
  def render: String =
    val quoted = if tag.startsWith("\"") then tag else s"\"$tag\""
    if weak then s"W/$quoted" else quoted

object EntityTag:
  def parse(raw: String): Option[EntityTag] =
    val s    = raw.trim
    val weak = s.regionMatches(true, 0, "W/", 0, 2)
    val rest = if weak then s.substring(2).trim else s
    if rest.length >= 2 && rest.charAt(0) == '"' && rest.charAt(rest.length - 1) == '"' then
      Some(EntityTag(rest.substring(1, rest.length - 1), weak))
    else if rest.nonEmpty && rest != "*" then Some(EntityTag(rest, weak))
    else None

  def parseList(raw: String): Chunk[EntityTag] =
    Chunk.fromIterable(
      raw.split(',').toList.map(_.trim).filter(_.nonEmpty).flatMap(parse)
    )
end EntityTag
