package heddle.http

import heddle.internal.Ascii
import zio.Chunk

/** RFC 9110 method token, PATCH, or an unlisted name. [[Method.parse]] is `None` only for empty input. */
enum Method:
  case GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH

  /** Wire token that is not a named RFC 9110 / PATCH method, e.g. `PROPFIND`. */
  case Custom(name: String)

  def render: String =
    this match
      case Custom(name) => name
      case named        => named.productPrefix

object Method:
  def parse(raw: String): Option[Method] =
    if raw.isEmpty then None
    else
      raw.toUpperCase match
        case "GET"     => Some(GET)
        case "HEAD"    => Some(HEAD)
        case "POST"    => Some(POST)
        case "PUT"     => Some(PUT)
        case "DELETE"  => Some(DELETE)
        case "CONNECT" => Some(CONNECT)
        case "OPTIONS" => Some(OPTIONS)
        case "TRACE"   => Some(TRACE)
        case "PATCH"   => Some(PATCH)
        case _         => Some(Custom(raw))

  def parse(raw: Chunk[Byte], from: Int, until: Int): Option[Method] =
    val n                             = until - from
    inline def is(s: String): Boolean = Ascii.eqIgnoreCase(raw, from, until, s)
    n match
      case 0                  => None
      case 3 if is("GET")     => Some(GET)
      case 3 if is("PUT")     => Some(PUT)
      case 4 if is("HEAD")    => Some(HEAD)
      case 4 if is("POST")    => Some(POST)
      case 5 if is("PATCH")   => Some(PATCH)
      case 5 if is("TRACE")   => Some(TRACE)
      case 6 if is("DELETE")  => Some(DELETE)
      case 7 if is("OPTIONS") => Some(OPTIONS)
      case 7 if is("CONNECT") => Some(CONNECT)
      case _                  => Some(Custom(Ascii.string(raw, from, until)))
  end parse
end Method
