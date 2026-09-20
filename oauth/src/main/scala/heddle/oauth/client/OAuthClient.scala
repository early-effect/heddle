package heddle.oauth.client

import heddle.client.Client
import heddle.crypto.{Base64Url, DigestPlatform}
import heddle.http.{Body, Form, Method, Request}
import heddle.http.header.{Authorization, HeaderName}
import heddle.http.header.Authorization.given
import heddle.oauth.OAuthError
import java.nio.charset.StandardCharsets
import zio.{Chunk, IO, UIO, ZIO}

final case class TokenSet(
    accessToken: String,
    tokenType: String,
    expiresIn: Option[Long],
    refreshToken: Option[String],
    idToken: Option[String],
    scope: Option[String],
)

final case class Pkce(verifier: String, challenge: String, method: String = "S256")

final case class AuthzRequest(
    redirectUri: String,
    scope: Set[String],
    state: String,
    extra: Map[String, String] = Map.empty,
)

final case class DeviceAuth(deviceCode: String, userCode: String, verificationUri: String, interval: Long)

trait OAuthClient:
  def authorizationUrl(req: AuthzRequest, pkce: Pkce): UIO[String]
  def exchange(code: String, redirectUri: String, pkce: Pkce): IO[OAuthError, TokenSet]
  def refresh(refreshToken: String): IO[OAuthError, TokenSet]
  def clientCredentials(scope: Set[String]): IO[OAuthError, TokenSet]
  def userInfo(accessToken: String): IO[OAuthError, String]
  def deviceStart(scope: Set[String]): IO[OAuthError, DeviceAuth]
  def devicePoll(device: DeviceAuth): IO[OAuthError, TokenSet]

object OAuthClient:
  def pkce(): Pkce =
    val verifier = Base64Url.encode(
      Chunk.fromArray(java.util.UUID.randomUUID().toString.getBytes(StandardCharsets.US_ASCII))
    )
    val digest = DigestPlatform.sha256Sync(Chunk.fromArray(verifier.getBytes(StandardCharsets.US_ASCII)))
    Pkce(verifier, Base64Url.encode(digest))

  def apply(
      client: Client,
      issuer: String,
      clientId: String,
      clientSecret: Option[String],
      authorizationEndpoint: String,
      tokenEndpoint: String,
  ): OAuthClient =
    apply(client, issuer, clientId, clientSecret, authorizationEndpoint, tokenEndpoint, None, None)

  def apply(
      client: Client,
      issuer: String,
      clientId: String,
      clientSecret: Option[String],
      authorizationEndpoint: String,
      tokenEndpoint: String,
      userInfoEndpoint: Option[String],
  ): OAuthClient =
    apply(client, issuer, clientId, clientSecret, authorizationEndpoint, tokenEndpoint, userInfoEndpoint, None)

  def apply(
      client: Client,
      issuer: String,
      clientId: String,
      clientSecret: Option[String],
      authorizationEndpoint: String,
      tokenEndpoint: String,
      userInfoEndpoint: Option[String],
      deviceAuthorizationEndpoint: Option[String],
  ): OAuthClient =
    Live(
      client,
      issuer,
      clientId,
      clientSecret,
      authorizationEndpoint,
      tokenEndpoint,
      userInfoEndpoint,
      deviceAuthorizationEndpoint,
    )

  private final class Live(
      http: Client,
      @annotation.unused issuer: String,
      clientId: String,
      clientSecret: Option[String],
      authorizationEndpoint: String,
      tokenEndpoint: String,
      userInfoEndpoint: Option[String],
      deviceAuthorizationEndpoint: Option[String],
  ) extends OAuthClient:
    def authorizationUrl(req: AuthzRequest, pkce: Pkce): UIO[String] =
      ZIO.succeed {
        val q = List(
          "response_type"         -> "code",
          "client_id"             -> clientId,
          "redirect_uri"          -> req.redirectUri,
          "scope"                 -> req.scope.mkString(" "),
          "state"                 -> req.state,
          "code_challenge"        -> pkce.challenge,
          "code_challenge_method" -> pkce.method,
        ) ++ req.extra.toList
        authorizationEndpoint + "?" + Form(q*).render
      }

    def exchange(code: String, redirectUri: String, pkce: Pkce): IO[OAuthError, TokenSet] =
      token(
        Form(
          "grant_type"    -> "authorization_code",
          "code"          -> code,
          "redirect_uri"  -> redirectUri,
          "code_verifier" -> pkce.verifier,
        )
      )

    def refresh(refreshToken: String): IO[OAuthError, TokenSet] =
      token(Form("grant_type" -> "refresh_token", "refresh_token" -> refreshToken))

    def clientCredentials(scope: Set[String]): IO[OAuthError, TokenSet] =
      token(Form("grant_type" -> "client_credentials", "scope" -> scope.mkString(" ")))

    def userInfo(accessToken: String): IO[OAuthError, String] =
      userInfoEndpoint match
        case None      => ZIO.fail(OAuthError.Protocol("userinfo_endpoint not configured"))
        case Some(url) =>
          val req = Request.get(url).addHeader(HeaderName.Authorization, s"Bearer $accessToken")
          http.batched(req).mapError(OAuthError.Transport.apply).flatMap { res =>
            if res.status.code == 200 then res.body.utf8.mapError(OAuthError.Transport.apply)
            else ZIO.fail(OAuthError.Protocol(s"userinfo ${res.status.code}"))
          }

    def deviceStart(scope: Set[String]): IO[OAuthError, DeviceAuth] =
      deviceAuthorizationEndpoint match
        case None      => ZIO.fail(OAuthError.Protocol("device_authorization_endpoint not configured"))
        case Some(url) =>
          postForm(url, Form("client_id" -> clientId, "scope" -> scope.mkString(" "))).map { json =>
            DeviceAuth(
              field(json, "device_code").getOrElse(""),
              field(json, "user_code").getOrElse(""),
              field(json, "verification_uri").getOrElse(""),
              field(json, "interval").flatMap(_.toLongOption).getOrElse(5L),
            )
          }

    def devicePoll(device: DeviceAuth): IO[OAuthError, TokenSet] =
      token(Form("grant_type" -> "urn:ietf:params:oauth:grant-type:device_code", "device_code" -> device.deviceCode))

    private def token(form: Form): IO[OAuthError, TokenSet] =
      postForm(tokenEndpoint, form).map(parseToken)

    private def postForm(url: String, form: Form): IO[OAuthError, String] =
      val bodyForm =
        if clientSecret.isEmpty then Form(form.fields :+ ("client_id" -> clientId)*) else form
      val base = Request(Method.POST, heddle.http.Url.parse(url), body = Body.form(bodyForm))
      val sent = clientSecret match
        case Some(secret) => base.copy(headers = base.headers.set(Authorization.basic(clientId, secret)))
        case None         => base
      http.batched(sent).mapError(OAuthError.Transport.apply).flatMap { res =>
        res.body.utf8.mapError(OAuthError.Transport.apply).flatMap { json =>
          if res.status.code / 100 == 2 then ZIO.succeed(json)
          else ZIO.fail(OAuthError.Protocol(s"token ${res.status.code}: $json"))
        }
      }
    end postForm

    private def parseToken(json: String): TokenSet =
      TokenSet(
        accessToken = field(json, "access_token").getOrElse(""),
        tokenType = field(json, "token_type").getOrElse("Bearer"),
        expiresIn = field(json, "expires_in").flatMap(_.toLongOption),
        refreshToken = field(json, "refresh_token"),
        idToken = field(json, "id_token"),
        scope = field(json, "scope"),
      )
  end Live

  private def field(json: String, name: String): Option[String] =
    val key = s"\"$name\""
    val i   = json.indexOf(key)
    if i < 0 then None
    else
      val colon = json.indexOf(':', i + key.length)
      if colon < 0 then None
      else
        var p = colon + 1
        while p < json.length && json.charAt(p).isWhitespace do p += 1
        if p >= json.length then None
        else if json.charAt(p) == '"' then
          val end = json.indexOf('"', p + 1)
          if end < 0 then None else Some(json.substring(p + 1, end))
        else
          val end = json.indexWhere(c => c == ',' || c == '}' || c.isWhitespace, p)
          val to  = if end < 0 then json.length else end
          Some(json.substring(p, to))
      end if
    end if
  end field
end OAuthClient
