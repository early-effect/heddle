package heddle.oauth.client

import heddle.client.Client
import heddle.crypto.{Base64Url, DigestPlatform}
import heddle.http.{Body, Form, Method, Request}
import heddle.http.header.{Authorization, HeaderName}
import heddle.http.header.Authorization.given
import heddle.internal.Ids
import heddle.oauth.{OAuthError, ProviderEndpoint}
import java.nio.charset.StandardCharsets
import zio.{Chunk, IO, UIO, ZIO}
import zio.json.*

final case class TokenSet(
    accessToken: String,
    tokenType: String,
    expiresIn: Option[Long],
    refreshToken: Option[String],
    idToken: Option[String],
    scope: Option[String],
)

/** RFC 7636 with S256, the only method heddle sends or accepts. */
final case class Pkce(verifier: String, challenge: String)

final case class AuthzRequest(
    redirectUri: String,
    scope: Set[String],
    state: String,
    extra: Map[String, String] = Map.empty,
)

final case class DeviceAuth(deviceCode: String, userCode: String, verificationUri: String, interval: Long)

/** How this client is registered with the authorization server. A public client has no secret. */
final case class Registration(clientId: String, clientSecret: Option[String] = None)

/** The authorization server's endpoints, as its discovery document lists them. */
final case class OAuthEndpoints(
    authorization: String,
    token: String,
    userInfo: Option[String] = None,
    deviceAuthorization: Option[String] = None,
)

trait OAuthClient:
  def authorizationUrl(req: AuthzRequest, pkce: Pkce): UIO[String]
  def exchange(code: String, redirectUri: String, pkce: Pkce): IO[OAuthError, TokenSet]
  def refresh(refreshToken: String): IO[OAuthError, TokenSet]
  def clientCredentials(scope: Set[String]): IO[OAuthError, TokenSet]
  def userInfo(accessToken: String): IO[OAuthError, String]
  def deviceStart(scope: Set[String]): IO[OAuthError, DeviceAuth]
  def devicePoll(device: DeviceAuth): IO[OAuthError, TokenSet]

object OAuthClient:
  /** A verifier of 32 CSPRNG bytes (RFC 7636 §4.1) and its S256 challenge. */
  val pkce: UIO[Pkce] =
    Ids.bytes(32).map { raw =>
      val verifier = Base64Url.encode(raw)
      val digest   = DigestPlatform.sha256Sync(Chunk.fromArray(verifier.getBytes(StandardCharsets.US_ASCII)))
      Pkce(verifier, Base64Url.encode(digest))
    }

  def apply(http: Client, registration: Registration, endpoints: OAuthEndpoints): OAuthClient =
    Live(http, registration, endpoints)

  private final class Live(http: Client, registration: Registration, endpoints: OAuthEndpoints) extends OAuthClient:
    def authorizationUrl(req: AuthzRequest, pkce: Pkce): UIO[String] =
      ZIO.succeed {
        val q = List(
          "response_type"         -> "code",
          "client_id"             -> registration.clientId,
          "redirect_uri"          -> req.redirectUri,
          "scope"                 -> req.scope.mkString(" "),
          "state"                 -> req.state,
          "code_challenge"        -> pkce.challenge,
          "code_challenge_method" -> "S256",
        ) ++ req.extra.toList
        endpoints.authorization + "?" + Form(q*).render
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
      ZIO.fromOption(endpoints.userInfo).orElseFail(OAuthError.NotConfigured(ProviderEndpoint.UserInfo)).flatMap {
        url =>
          val req = Request.get(url).addHeader(HeaderName.Authorization, s"Bearer $accessToken")
          send(req, ProviderEndpoint.UserInfo)
      }

    def deviceStart(scope: Set[String]): IO[OAuthError, DeviceAuth] =
      ZIO
        .fromOption(endpoints.deviceAuthorization)
        .orElseFail(OAuthError.NotConfigured(ProviderEndpoint.DeviceAuthorization))
        .flatMap { url =>
          postForm(
            url,
            Form("client_id" -> registration.clientId, "scope" -> scope.mkString(" ")),
            ProviderEndpoint.DeviceAuthorization,
          )
            .flatMap(decode[DeviceWire](_, ProviderEndpoint.DeviceAuthorization))
            .map(w => DeviceAuth(w.device_code, w.user_code, w.verification_uri, w.interval.getOrElse(5L)))
        }

    def devicePoll(device: DeviceAuth): IO[OAuthError, TokenSet] =
      token(Form("grant_type" -> "urn:ietf:params:oauth:grant-type:device_code", "device_code" -> device.deviceCode))

    private def token(form: Form): IO[OAuthError, TokenSet] =
      postForm(endpoints.token, form, ProviderEndpoint.Token)
        .flatMap(decode[TokenWire](_, ProviderEndpoint.Token))
        .map { w =>
          TokenSet(w.access_token, w.token_type.getOrElse("Bearer"), w.expires_in, w.refresh_token, w.id_token, w.scope)
        }

    /** A confidential client authenticates with HTTP Basic (RFC 6749 §2.3.1); a public one names itself in the form. */
    private def postForm(url: String, form: Form, endpoint: ProviderEndpoint): IO[OAuthError, String] =
      val base = Request(Method.POST, heddle.http.Url.parse(url))
      val req  = registration.clientSecret match
        case Some(secret) =>
          base
            .copy(body = Body.form(form))
            .copy(headers = base.headers.set(Authorization.basic(registration.clientId, secret)))
        case None => base.copy(body = Body.form(Form(form.fields :+ ("client_id" -> registration.clientId)*)))
      send(req, endpoint)

    private def send(req: Request, endpoint: ProviderEndpoint): IO[OAuthError, String] =
      http.batched(req).mapError(OAuthError.Transport(_)).flatMap { res =>
        res.body.utf8.mapError(OAuthError.Transport(_)).flatMap { body =>
          if res.status.isSuccess then ZIO.succeed(body) else ZIO.fail(OAuthError.Refused(endpoint, res.status, body))
        }
      }
  end Live

  private def decode[A: JsonDecoder](json: String, endpoint: ProviderEndpoint): IO[OAuthError, A] =
    ZIO.fromEither(json.fromJson[A]).mapError(OAuthError.BadResponse(endpoint, _))

  /** RFC 6749 §5.1, field for field. */
  private final case class TokenWire(
      access_token: String,
      token_type: Option[String],
      expires_in: Option[Long],
      refresh_token: Option[String],
      id_token: Option[String],
      scope: Option[String],
  ) derives JsonDecoder

  /** RFC 8628 §3.2. */
  private final case class DeviceWire(
      device_code: String,
      user_code: String,
      verification_uri: String,
      interval: Option[Long],
  ) derives JsonDecoder
end OAuthClient
