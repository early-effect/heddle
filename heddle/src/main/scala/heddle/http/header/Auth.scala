package heddle.http.header

import heddle.error.ParamError

enum AuthScheme:
  case Basic, Bearer
  case Other(token: String)

  def render: String =
    this match
      case Basic    => "Basic"
      case Bearer   => "Bearer"
      case Other(t) => t

object AuthScheme:
  def parse(raw: String): AuthScheme =
    raw.toLowerCase match
      case "basic"  => Basic
      case "bearer" => Bearer
      case _        => Other(raw)

final case class BasicCredentials(username: String, password: String)

object BasicCredentials:
  /** RFC 7617 §2: base64 of `user-id ":" password`, split at the first colon. */
  def parse(credentials: String): Option[BasicCredentials] =
    heddle.crypto.Base64.decode(credentials.trim).toOption.flatMap { bytes =>
      val raw   = String(bytes.toArray, java.nio.charset.StandardCharsets.UTF_8)
      val colon = raw.indexOf(':')
      Option.when(colon >= 0)(BasicCredentials(raw.substring(0, colon), raw.substring(colon + 1)))
    }

  def parse(auth: Authorization): Option[BasicCredentials] =
    auth.scheme match
      case AuthScheme.Basic => parse(auth.credentials)
      case _                => None

  def render(username: String, password: String): String =
    heddle.crypto.Base64.encode(
      zio.Chunk.fromArray(s"$username:$password".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    )

  /** `Authorization` whose scheme is `Basic` and whose credentials decode. */
  given TypedHeader[BasicCredentials] =
    TypedHeader.single(HeaderName.Authorization)(
      raw =>
        val auth = Authorization.parse(raw)
        auth.scheme match
          case AuthScheme.Basic => parse(auth.credentials).toRight(ParamError.Malformed(raw, "basic credentials"))
          case _                => Left(ParamError.Malformed(raw, "a basic authorization"))
      ,
      creds => s"Basic ${render(creds.username, creds.password)}",
    )
end BasicCredentials

final case class Authorization(scheme: AuthScheme, credentials: String)

object Authorization:
  def basic(username: String, password: String): Authorization =
    Authorization(AuthScheme.Basic, BasicCredentials.render(username, password))

  def bearer(token: String): Authorization =
    Authorization(AuthScheme.Bearer, token)

  def parse(raw: String): Authorization =
    val s     = raw.trim
    val space = s.indexOf(' ')
    if space < 0 then Authorization(AuthScheme.parse(s), "")
    else Authorization(AuthScheme.parse(s.substring(0, space)), s.substring(space + 1))

  given TypedHeader[Authorization] =
    TypedHeader.single(HeaderName.Authorization)(
      raw => Right(parse(raw)),
      auth => if auth.credentials.isEmpty then auth.scheme.render else s"${auth.scheme.render} ${auth.credentials}",
    )
end Authorization

/** A bearer token. The scheme must be `Bearer` and the token must be non-empty. */
final case class BearerToken(token: String)

object BearerToken:
  given TypedHeader[BearerToken] =
    TypedHeader.single(HeaderName.Authorization)(
      raw =>
        val auth = Authorization.parse(raw)
        auth.scheme match
          case AuthScheme.Bearer if auth.credentials.nonEmpty => Right(BearerToken(auth.credentials))
          case AuthScheme.Bearer                              => Left(ParamError.Malformed(raw, "a bearer token"))
          case _ => Left(ParamError.Malformed(raw, "a bearer authorization"))
      ,
      token => s"Bearer ${token.token}",
    )
end BearerToken

final case class WwwAuthenticate(scheme: AuthScheme, params: List[(String, String)])

object WwwAuthenticate:
  def parse(raw: String): WwwAuthenticate =
    val s     = raw.trim
    val space = s.indexOf(' ')
    if space < 0 then WwwAuthenticate(AuthScheme.parse(s), Nil)
    else
      val scheme = AuthScheme.parse(s.substring(0, space))
      WwwAuthenticate(scheme, params(s.substring(space + 1)))

  private def params(rest: String): List[(String, String)] =
    val out  = List.newBuilder[(String, String)]
    var from = 0
    val s    = rest.trim
    while from < s.length do
      while from < s.length && (s.charAt(from) == ' ' || s.charAt(from) == ',') do from += 1
      if from >= s.length then ()
      else
        val eq = s.indexOf('=', from)
        if eq < 0 then from = s.length
        else
          val k = s.substring(from, eq).trim
          var i = eq + 1
          while i < s.length && s.charAt(i) == ' ' do i += 1
          val (v, next) =
            if i < s.length && s.charAt(i) == '"' then
              val close = s.indexOf('"', i + 1)
              if close < 0 then (s.substring(i + 1), s.length)
              else (s.substring(i + 1, close), close + 1)
            else
              val comma = s.indexOf(',', i)
              val end   = if comma < 0 then s.length else comma
              (s.substring(i, end).trim, end)
          if k.nonEmpty then out += (k -> v)
          from = next
        end if
      end if
    end while
    out.result()
  end params

  given TypedHeader[WwwAuthenticate] =
    TypedHeader.single(HeaderName.WwwAuthenticate)(
      raw => Right(parse(raw)),
      header =>
        if header.params.isEmpty then header.scheme.render
        else
          val ps = header.params.map((k, v) => s"""$k="$v"""").mkString(", ")
          s"${header.scheme.render} $ps",
    )
end WwwAuthenticate
