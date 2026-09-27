package heddle.internal

import java.nio.charset.StandardCharsets
import zio.Chunk

private[heddle] object Ascii:
  val Crlf: Array[Byte]        = Array[Byte]('\r', '\n')
  val ColonSpace: Array[Byte]  = Array[Byte](':', ' ')
  val Http11Sp: Array[Byte]    = "HTTP/1.1 ".getBytes(StandardCharsets.US_ASCII)
  val ChunkedEnd: Array[Byte]  = "0\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
  val Connection: String       = "Connection"
  val ContentLength: String    = "Content-Length"
  val ContentType: String      = "Content-Type"
  val TransferEncoding: String = "Transfer-Encoding"
  val Host: String             = "Host"
  val UserAgent: String        = "User-Agent"
  val Accept: String           = "Accept"
  val Close: String            = "close"
  val KeepAlive: String        = "keep-alive"
  val Chunked: String          = "chunked"

  def eq(raw: Chunk[Byte], from: Int, until: Int, s: String): Boolean =
    val n = until - from
    if n != s.length then false
    else
      var i = 0
      while i < n && (raw(from + i) & 0xff) == (s.charAt(i) & 0xff) do i += 1
      i == n

  def eq(raw: Array[Byte], from: Int, until: Int, s: String): Boolean =
    val n = until - from
    if n != s.length then false
    else
      var i = 0
      while i < n && (raw(from + i) & 0xff) == (s.charAt(i) & 0xff) do i += 1
      i == n

  def eqIgnoreCase(raw: Array[Byte], from: Int, until: Int, s: String): Boolean =
    val n = until - from
    if n != s.length then false
    else
      var i = 0
      while i < n && lower(raw(from + i) & 0xff) == lower(s.charAt(i) & 0xff) do i += 1
      i == n

  def eqIgnoreCase(raw: Chunk[Byte], from: Int, until: Int, s: String): Boolean =
    val n = until - from
    if n != s.length then false
    else
      var i = 0
      while i < n && lower(raw(from + i) & 0xff) == lower(s.charAt(i) & 0xff) do i += 1
      i == n

  def internName(raw: Array[Byte], from: Int, until: Int): String =
    if eqIgnoreCase(raw, from, until, Connection) then Connection
    else if eqIgnoreCase(raw, from, until, ContentLength) then ContentLength
    else if eqIgnoreCase(raw, from, until, ContentType) then ContentType
    else if eqIgnoreCase(raw, from, until, TransferEncoding) then TransferEncoding
    else if eqIgnoreCase(raw, from, until, Host) then Host
    else if eqIgnoreCase(raw, from, until, UserAgent) then UserAgent
    else if eqIgnoreCase(raw, from, until, Accept) then Accept
    else string(raw, from, until)

  /** Known values interned; unknown returns null so the caller can keep a slice. */
  def internValue(raw: Array[Byte], from: Int, until: Int): String | Null =
    val (a, b) = trim(raw, from, until)
    if eqIgnoreCase(raw, a, b, Close) then Close
    else if eqIgnoreCase(raw, a, b, KeepAlive) then KeepAlive
    else if eqIgnoreCase(raw, a, b, Chunked) then Chunked
    else null

  def string(raw: Chunk[Byte], from: Int, until: Int): String =
    val len = until - from
    if len <= 0 then ""
    else
      val arr = Array.ofDim[Byte](len)
      var k   = 0
      while k < len do
        arr(k) = raw(from + k)
        k += 1
      String(arr, StandardCharsets.US_ASCII)
  end string

  def string(raw: Array[Byte], from: Int, until: Int): String =
    val len = until - from
    if len <= 0 then ""
    else String(raw, from, len, StandardCharsets.US_ASCII)

  def trim(raw: Array[Byte], from: Int, until: Int): (Int, Int) =
    var a = from
    var b = until
    while a < b && (raw(a) == ' ' || raw(a) == '\t') do a += 1
    while b > a && (raw(b - 1) == ' ' || raw(b - 1) == '\t') do b -= 1
    (a, b)

  def trim(raw: Chunk[Byte], from: Int, until: Int): (Int, Int) =
    var a = from
    var b = until
    while a < b && (raw(a) == ' ' || raw(a) == '\t') do a += 1
    while b > a && (raw(b - 1) == ' ' || raw(b - 1) == '\t') do b -= 1
    (a, b)

  /** RFC 9110 `1*DIGIT` (a Content-Length): ASCII digits only, no sign, no whitespace, no overflow. */
  def decimal(s: String): Option[Long] = unsigned(s, 10)

  /** RFC 9112 `1*HEXDIG` (a chunk size): ASCII hex digits only, no sign, no whitespace, no overflow. */
  def hex(s: String): Option[Long] = unsigned(s, 16)

  private def unsigned(s: String, radix: Int): Option[Long] =
    var n  = 0L
    var i  = 0
    var ok = s.nonEmpty
    while ok && i < s.length do
      val d = digit(s.charAt(i))
      ok = d >= 0 && d < radix && n <= (Long.MaxValue - d) / radix
      if ok then n = n * radix + d
      i += 1
    Option.when(ok)(n)
  end unsigned

  private def digit(c: Char): Int =
    if c >= '0' && c <= '9' then c - '0'
    else if c >= 'a' && c <= 'f' then c - 'a' + 10
    else if c >= 'A' && c <= 'F' then c - 'A' + 10
    else -1

  private def lower(c: Int): Int =
    if c >= 'A' && c <= 'Z' then c + 32 else c
end Ascii
