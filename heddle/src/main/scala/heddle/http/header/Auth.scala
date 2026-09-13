package heddle.http.header

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
  def parse(credentials: String): Option[BasicCredentials] =
    try
      val raw   = String(java.util.Base64.getDecoder.decode(credentials.trim), java.nio.charset.StandardCharsets.UTF_8)
      val colon = raw.indexOf(':')
      if colon < 0 then None
      else Some(BasicCredentials(raw.substring(0, colon), raw.substring(colon + 1)))
    catch case _: IllegalArgumentException => None

  def parse(auth: Authorization): Option[BasicCredentials] =
    auth.scheme match
      case AuthScheme.Basic => parse(auth.credentials)
      case _                => None

  def render(username: String, password: String): String =
    java.util.Base64.getEncoder.encodeToString(s"$username:$password".getBytes(java.nio.charset.StandardCharsets.UTF_8))
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

  given TypedHeader[Authorization] with
    def name: HeaderName                                   = HeaderName.Authorization
    def decode(raw: String): Either[String, Authorization] = Right(parse(raw))
    def encode(a: Authorization): String                   =
      if a.credentials.isEmpty then a.scheme.render else s"${a.scheme.render} ${a.credentials}"
end Authorization

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

  given TypedHeader[WwwAuthenticate] with
    def name: HeaderName                                     = HeaderName.WwwAuthenticate
    def decode(raw: String): Either[String, WwwAuthenticate] = Right(parse(raw))
    def encode(a: WwwAuthenticate): String                   =
      if a.params.isEmpty then a.scheme.render
      else
        val ps = a.params.map((k, v) => s"$k=\"$v\"").mkString(", ")
        s"${a.scheme.render} $ps"
end WwwAuthenticate
