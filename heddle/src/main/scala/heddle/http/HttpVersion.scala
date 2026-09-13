package heddle.http

import heddle.internal.Ascii
import zio.Chunk

enum HttpVersion(val render: String):
  case Http10 extends HttpVersion("HTTP/1.0")
  case Http11 extends HttpVersion("HTTP/1.1")

  /** HTTP/2 (`"HTTP/2"`). h2 requests carry this, not [[Http11]]. */
  case Http2              extends HttpVersion("HTTP/2")
  case Other(raw: String) extends HttpVersion(raw)

object HttpVersion:
  def parse(raw: Chunk[Byte], from: Int, until: Int): HttpVersion =
    if Ascii.eq(raw, from, until, Http11.render) then Http11
    else if Ascii.eq(raw, from, until, Http10.render) then Http10
    else Other(Ascii.string(raw, from, until))
