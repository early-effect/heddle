package heddle.http

import heddle.internal.Ascii
import zio.Chunk

/** A request target: origin-form (`/path?query`) or absolute-form (`scheme://host[:port]/path?query`).
  *
  * `host` keeps IPv6 literals bracketed (`[::1]`), as they appear in an authority. `port` is `None` when the scheme's
  * default applies.
  */
final case class Url(
    path: Path,
    query: QueryParams = QueryParams.empty,
    scheme: Option[Scheme] = None,
    host: Option[String] = None,
    port: Option[Int] = None,
):
  def /(segment: String): Url = copy(path = path / segment)

  def absolute: Boolean = host.isDefined

  def render: String =
    val pq =
      if query.isEmpty then path.render
      else s"${path.render}?${query.render}"
    host match
      case None    => pq
      case Some(h) =>
        val sch = scheme.getOrElse(Scheme.Http)
        val ps  = port.filterNot(_ == sch.defaultPort).map(n => s":$n").getOrElse("")
        s"${sch.render}://$h$ps$pq"
  end render
end Url

object Url:
  val root: Url = Url(Path.root)

  /** Total. Absolute-form needs a known scheme and a non-empty host; anything else is read as origin-form. */
  def parse(raw: String): Url =
    val noFragment = raw.indexOf('#') match
      case -1 => raw
      case i  => raw.substring(0, i)
    absolute(noFragment).getOrElse(origin(noFragment))

  /** Strict: every character must be legal in a URI reference (RFC 3986 §2), and absolute-form needs a known scheme, a
    * well-formed host, and a numeric port. Use this for URLs that arrive as data (headers, config, user input).
    */
  def decode(raw: String): Either[String, Url] =
    val noFragment = raw.indexOf('#') match
      case -1 => raw
      case i  => raw.substring(0, i)
    if raw.isEmpty then Left("empty URL")
    else if !raw.forall(uriChar) then Left(s"illegal character in URL: $raw")
    else
      val sep = noFragment.indexOf("://")
      if sep <= 0 then Right(origin(noFragment))
      else
        absolute(noFragment) match
          case Some(url) if url.host.forall(validHost) && portIsNumeric(noFragment, sep) => Right(url)
          case _ => Left(s"malformed absolute URL: $raw")
  end decode

  def parse(raw: Chunk[Byte], from: Int, until: Int): Url =
    var q = from
    while q < until && raw(q) != '?' do q += 1
    val path = Path.decode(raw, from, q)
    if q < until then Url(path, QueryParams.decode(Ascii.string(raw, q + 1, until)))
    else Url(path, QueryParams.empty)

  private def origin(raw: String): Url =
    val bytes = Chunk.fromArray(raw.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    parse(bytes, 0, bytes.length)

  private def absolute(raw: String): Option[Url] =
    val sep = raw.indexOf("://")
    if sep <= 0 then None
    else
      Scheme.parse(raw.substring(0, sep)).flatMap { scheme =>
        val rest = raw.substring(sep + 3)
        val end  = rest.indexWhere(c => c == '/' || c == '?') match
          case -1 => rest.length
          case i  => i
        val userAndHost  = rest.substring(0, end)
        val authority    = userAndHost.substring(userAndHost.lastIndexOf('@') + 1)
        val tail         = rest.substring(end)
        val (host, port) = splitAuthority(authority)
        Option.when(host.nonEmpty) {
          val target = origin(if tail.startsWith("/") then tail else "/" + tail)
          target.copy(scheme = Some(scheme), host = Some(host), port = port.filterNot(_ == scheme.defaultPort))
        }
      }
    end if
  end absolute

  private def uriChar(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || "-._~:/?#[]@!$&'()*+,;=%".contains(c)

  private def validHost(host: String): Boolean =
    if host.startsWith("[") then
      host.endsWith("]") && host.length > 2 && host
        .drop(1)
        .dropRight(1)
        .forall(c => c.isLetterOrDigit || c == ':' || c == '.')
    else host.nonEmpty && host.forall(c => c.isLetterOrDigit || "-._~!$&'()*+,;=%".contains(c))

  private def portIsNumeric(raw: String, sep: Int): Boolean =
    val rest = raw.substring(sep + 3)
    val end  = rest.indexWhere(c => c == '/' || c == '?') match
      case -1 => rest.length
      case i  => i
    val authority = rest.substring(rest.lastIndexOf('@', end) + 1, end)
    val bracket   = authority.lastIndexOf(']')
    val colon     = authority.lastIndexOf(':')
    colon <= bracket || {
      val digits = authority.substring(colon + 1)
      digits.nonEmpty && digits.forall(_.isDigit) && digits.toIntOption.exists(p => p >= 0 && p <= 65535)
    }
  end portIsNumeric

  private def splitAuthority(authority: String): (String, Option[Int]) =
    val bracket = authority.lastIndexOf(']')
    val colon   = authority.lastIndexOf(':')
    if colon <= bracket then (authority, None)
    else
      val digits = authority.substring(colon + 1)
      digits.toIntOption.filter(p => p >= 0 && p <= 65535 && digits.forall(_.isDigit)) match
        case Some(p) => (authority.substring(0, colon), Some(p))
        case None    => (authority, None)
end Url
