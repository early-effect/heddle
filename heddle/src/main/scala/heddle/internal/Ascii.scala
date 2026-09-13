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
      while i < n do
        if lower(raw(from + i) & 0xff) != lower(s.charAt(i) & 0xff) then return false
        i += 1
      true

  def eqIgnoreCase(raw: Chunk[Byte], from: Int, until: Int, s: String): Boolean =
    val n = until - from
    if n != s.length then false
    else
      var i = 0
      while i < n do
        if lower(raw(from + i) & 0xff) != lower(s.charAt(i) & 0xff) then return false
        i += 1
      true

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

  private def lower(c: Int): Int =
    if c >= 'A' && c <= 'Z' then c + 32 else c
end Ascii
