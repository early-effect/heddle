package heddle.http.header

import java.time.Instant
import zio.Chunk

final case class CookiePair(name: String, value: String)

object CookiePair:
  def parseHeader(raw: String): Chunk[CookiePair] =
    Chunk.fromIterable(
      raw
        .split(';')
        .toList
        .map(_.trim)
        .flatMap { crumb =>
          if crumb.isEmpty then None
          else
            val eq     = crumb.indexOf('=')
            val (n, v) =
              if eq < 0 then (crumb, "")
              else (crumb.substring(0, eq).trim, unquote(crumb.substring(eq + 1).trim))
            if n.isEmpty then None else Some(CookiePair(n, v))
        }
    )

  private[header] def unquote(value: String): String =
    if value.length >= 2 && value.charAt(0) == '"' && value.charAt(value.length - 1) == '"' then
      value.substring(1, value.length - 1)
    else value
end CookiePair

enum SameSite:
  case Lax, Strict, None
  def render: String =
    this match
      case Lax    => "Lax"
      case Strict => "Strict"
      case None   => "None"

object SameSite:
  def parse(raw: String): Option[SameSite] =
    raw.toLowerCase match
      case "lax"    => Some(Lax)
      case "strict" => Some(Strict)
      case "none"   => Some(SameSite.None)
      case _        => Option.empty

/** A `Set-Cookie` attribute that is present or absent (RFC 6265bis §5.6; `Partitioned` is CHIPS). */
enum CookieFlag(val attribute: String):
  case Secure      extends CookieFlag("Secure")
  case HttpOnly    extends CookieFlag("HttpOnly")
  case Partitioned extends CookieFlag("Partitioned")

final case class SetCookie(
    name: String,
    value: String,
    domain: Option[String] = None,
    path: Option[String] = None,
    maxAge: Option[Long] = None,
    expires: Option[Instant] = None,
    flags: Set[CookieFlag] = Set.empty,
    sameSite: Option[SameSite] = Option.empty,
)

object SetCookie:
  def parse(raw: String): Option[SetCookie] =
    raw.split(';').toList.map(_.trim).filter(_.nonEmpty) match
      case Nil            => None
      case first :: attrs =>
        val eq = first.indexOf('=')
        Option.when(eq > 0) {
          val base = SetCookie(first.substring(0, eq).trim, CookiePair.unquote(first.substring(eq + 1).trim))
          attrs.foldLeft(base)(attribute)
        }
  end parse

  /** An unknown attribute is ignored, and a malformed date or age is absent, as RFC 6265 §5.2 asks. */
  private def attribute(cookie: SetCookie, attr: String): SetCookie =
    val aeq    = attr.indexOf('=')
    val (k, v) =
      if aeq < 0 then (attr, "")
      else (attr.substring(0, aeq).trim, CookiePair.unquote(attr.substring(aeq + 1).trim))
    k.toLowerCase match
      case "domain"      => cookie.copy(domain = Some(v))
      case "path"        => cookie.copy(path = Some(v))
      case "max-age"     => cookie.copy(maxAge = v.toLongOption)
      case "expires"     => cookie.copy(expires = HttpDate.parse(v))
      case "secure"      => cookie.copy(flags = cookie.flags + CookieFlag.Secure)
      case "httponly"    => cookie.copy(flags = cookie.flags + CookieFlag.HttpOnly)
      case "partitioned" => cookie.copy(flags = cookie.flags + CookieFlag.Partitioned)
      case "samesite"    => cookie.copy(sameSite = SameSite.parse(v))
      case _             => cookie
    end match
  end attribute

  def render(cookie: SetCookie): String =
    val b = StringBuilder(quotePair(cookie.name, cookie.value))
    cookie.domain.foreach(d => b.append("; Domain=").append(d))
    cookie.path.foreach(p => b.append("; Path=").append(p))
    cookie.maxAge.foreach(n => b.append("; Max-Age=").append(n.toString))
    cookie.expires.foreach(t => b.append("; Expires=").append(HttpDate.render(t)))
    CookieFlag.values.filter(cookie.flags).foreach(f => b.append("; ").append(f.attribute))
    cookie.sameSite.foreach(s => b.append("; SameSite=").append(s.render))
    b.toString
  end render

  private def quotePair(name: String, value: String): String =
    if needsQuote(value) then s"""$name="${value.replace("\"", "")}""""
    else s"$name=$value"

  private def needsQuote(value: String): Boolean =
    value.exists(c => c == ' ' || c == ',' || c == ';' || c == '"')
end SetCookie
