package heddle.http

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import heddle.internal.Ascii
import heddle.route.PathLits
import zio.Chunk

final case class Path(segments: Chunk[String]):
  def render: String =
    if segments.isEmpty then "/" else segments.map(Path.percentEncode).mkString("/", "/", "")

  def /(segment: String): Path = Path(segments :+ segment)

  def isEmpty: Boolean = segments.isEmpty

object Path:
  val root: Path = Path(Chunk.empty)

  def decode(raw: String): Path =
    val bytes = Chunk.fromArray(raw.getBytes(StandardCharsets.UTF_8))
    decode(bytes, 0, bytes.length)

  def decode(raw: Chunk[Byte], from: Int, until: Int): Path =
    var q = from
    while q < until && raw(q) != '?' do q += 1
    var start = from
    if start < q && raw(start) == '/' then start += 1
    if start >= q then root
    else
      var slash = start
      while slash < q && raw(slash) != '/' do slash += 1
      if slash >= q then
        segmentAt(raw, start, q) match
          case None    => root
          case Some(s) => Path(Chunk.single(s))
      else
        val buf   = Array.newBuilder[String]
        var begin = start
        var i     = start
        while i < q do
          if raw(i) == '/' then
            segmentAt(raw, begin, i).foreach(buf += _)
            begin = i + 1
          i += 1
        segmentAt(raw, begin, q).foreach(buf += _)
        val arr = buf.result()
        if arr.isEmpty then root else Path(Chunk.fromArray(arr))
      end if
    end if
  end decode

  private def segmentAt(raw: Chunk[Byte], from: Int, until: Int): Option[String] =
    if until <= from then None
    else if until - from == 1 && raw(from) == '.' then None
    else
      var i   = from
      var enc = false
      while i < until && !enc do
        val c = raw(i)
        if c == '%' || c == '+' then enc = true
        i += 1
      Some(if enc then percentDecode(Ascii.string(raw, from, until)) else PathLits.intern(raw, from, until))

  def percentDecode(s: String): String =
    URLDecoder.decode(s.replace("+", "%2B"), "UTF-8")

  def percentEncode(s: String): String =
    s.flatMap { c =>
      if c.isLetterOrDigit || "-._~".contains(c) then c.toString
      else f"%%${c.toInt}%02X"
    }
end Path
