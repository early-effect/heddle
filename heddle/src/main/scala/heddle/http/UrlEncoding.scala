package heddle.http

import java.nio.charset.StandardCharsets

/** Percent-encoding that matches the JDK on UTF-8, without `URLEncoder` / `URLDecoder`.
  *
  * [[encode]] / [[decode]] are RFC 3986 unreserved (`-._~`). Space is `%20`. Plus is `%2B`. [[encodeForm]] /
  * [[decodeForm]] are `application/x-www-form-urlencoded` as `URLEncoder.encode` / `URLDecoder.decode`: space is `+`,
  * `*` stays, `~` is `%7E`.
  */
object UrlEncoding:
  def encode(s: String): String     = encodeBytes(s, form = false)
  def decode(s: String): String     = decodeBytes(s, plusIsSpace = false)
  def encodeForm(s: String): String = encodeBytes(s, form = true)
  def decodeForm(s: String): String = decodeBytes(s, plusIsSpace = true)

  private def encodeBytes(s: String, form: Boolean): String =
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    val out   = new java.lang.StringBuilder(bytes.length)
    var i     = 0
    while i < bytes.length do
      val b = bytes(i) & 0xff
      if form && b == ' ' then out.append('+')
      else if keep(b, form) then out.append(b.toChar)
      else
        out.append('%')
        out.append(hex((b >> 4) & 0xf))
        out.append(hex(b & 0xf))
      i += 1
    out.toString
  end encodeBytes

  private def decodeBytes(s: String, plusIsSpace: Boolean): String =
    val n     = s.length
    val bytes = Array.newBuilder[Byte]
    var i     = 0
    while i < n do
      s.charAt(i) match
        case '+' if plusIsSpace =>
          bytes += ' '.toByte
          i += 1
        case '%' =>
          if i + 2 >= n then throw IllegalArgumentException("incomplete percent-escape")
          val hi = fromHex(s.charAt(i + 1))
          val lo = fromHex(s.charAt(i + 2))
          if hi < 0 || lo < 0 then throw IllegalArgumentException("illegal percent-escape")
          bytes += ((hi << 4) | lo).toByte
          i += 3
        case c if c < 128 =>
          bytes += c.toByte
          i += 1
        case c =>
          bytes ++= String.valueOf(c).getBytes(StandardCharsets.UTF_8)
          i += 1
    end while
    String(bytes.result(), StandardCharsets.UTF_8)
  end decodeBytes

  private def keep(b: Int, form: Boolean): Boolean =
    (b >= 'A' && b <= 'Z') || (b >= 'a' && b <= 'z') || (b >= '0' && b <= '9') ||
      b == '-' || b == '.' || b == '_' || (if form then b == '*' else b == '~')

  private def hex(n: Int): Char =
    if n < 10 then ('0' + n).toChar else ('A' + n - 10).toChar

  private def fromHex(c: Char): Int =
    if c >= '0' && c <= '9' then c - '0'
    else if c >= 'A' && c <= 'F' then c - 'A' + 10
    else if c >= 'a' && c <= 'f' then c - 'a' + 10
    else -1
end UrlEncoding
