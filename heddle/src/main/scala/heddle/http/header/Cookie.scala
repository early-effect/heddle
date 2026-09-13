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

final case class SetCookie(
    name: String,
    value: String,
    domain: Option[String] = None,
    path: Option[String] = None,
    maxAge: Option[Long] = None,
    expires: Option[Instant] = None,
    secure: Boolean = false,
    httpOnly: Boolean = false,
    sameSite: Option[SameSite] = Option.empty,
)

object SetCookie:
  def parse(raw: String): Option[SetCookie] =
    val parts = raw.split(';').toList.map(_.trim).filter(_.nonEmpty)
    parts.headOption.flatMap { first =>
      val eq = first.indexOf('=')
      if eq <= 0 then None
      else
        val name                       = first.substring(0, eq).trim
        val value                      = CookiePair.unquote(first.substring(eq + 1).trim)
        var domain: Option[String]     = None
        var path: Option[String]       = None
        var maxAge: Option[Long]       = None
        var expires: Option[Instant]   = None
        var secure                     = false
        var httpOnly                   = false
        var sameSite: Option[SameSite] = Option.empty
        parts.tail.foreach { attr =>
          val aeq    = attr.indexOf('=')
          val (k, v) =
            if aeq < 0 then (attr, "")
            else (attr.substring(0, aeq).trim, CookiePair.unquote(attr.substring(aeq + 1).trim))
          k.toLowerCase match
            case "domain"   => domain = Some(v)
            case "path"     => path = Some(v)
            case "max-age"  => maxAge = v.toLongOption
            case "expires"  => expires = HttpDate.parse(v)
            case "secure"   => secure = true
            case "httponly" => httpOnly = true
            case "samesite" => sameSite = SameSite.parse(v)
            case _          => ()
        }
        Some(SetCookie(name, value, domain, path, maxAge, expires, secure, httpOnly, sameSite))
      end if
    }
  end parse

  def render(cookie: SetCookie): String =
    val b = StringBuilder(quotePair(cookie.name, cookie.value))
    cookie.domain.foreach(d => b.append("; Domain=").append(d))
    cookie.path.foreach(p => b.append("; Path=").append(p))
    cookie.maxAge.foreach(n => b.append("; Max-Age=").append(n.toString))
    cookie.expires.foreach(t => b.append("; Expires=").append(HttpDate.render(t)))
    if cookie.secure then b.append("; Secure")
    if cookie.httpOnly then b.append("; HttpOnly")
    cookie.sameSite.foreach(s => b.append("; SameSite=").append(s.render))
    b.toString
  end render

  private def quotePair(name: String, value: String): String =
    if needsQuote(value) then s"""$name="${value.replace("\"", "")}""""
    else s"$name=$value"

  private def needsQuote(value: String): Boolean =
    value.exists(c => c == ' ' || c == ',' || c == ';' || c == '"')
end SetCookie
