package heddle.oauth.rs

import heddle.client.Client
import heddle.crypto.Rsa
import heddle.http.Request
import heddle.oauth.{OAuthError, ProviderEndpoint}
import heddle.oauth.jose.{Expected, Jose, JoseError, Jwks}
import java.time.Instant
import zio.{Clock, IO, Ref, Schedule, Scope, ZEnvironment, ZIO, ZLayer, durationInt}
import zio.json.*

trait JwtVerifier:
  def verify(token: String): IO[OAuthError, JwtClaim]

object JwtVerifier:
  /** A verifier for a fixed key set. The audience is required: a resource server that skips it accepts any client's
    * token.
    */
  def static(jwksJson: String, issuer: String, audience: String): ZIO[Rsa, OAuthError, JwtVerifier] =
    ZIO.fromEither(Jose.parseJwks(jwksJson)).mapError(OAuthError.InvalidToken(_)).flatMap { jwks =>
      ZIO.serviceWith[Rsa](rsa => Static(jwks, Expected(issuer, Some(audience)), rsa))
    }

  def staticLayer(jwksJson: String, issuer: String, audience: String): ZLayer[Rsa, OAuthError, JwtVerifier] =
    ZLayer.fromZIO(static(jwksJson, issuer, audience))

  /** Keys from `jwksUri`, fetched before the layer is built and again every five minutes while it lives. */
  def jwks(jwksUri: String, issuer: String, audience: String): ZLayer[Client & Rsa, OAuthError, JwtVerifier] =
    ZLayer.scoped(remote(jwksUri, Expected(issuer, Some(audience))))

  /** Keys from the `jwks_uri` the issuer's discovery document names. */
  def issuer(issuer: String, audience: String): ZLayer[Client & Rsa, OAuthError, JwtVerifier] =
    ZLayer.scoped(
      ZIO.serviceWithZIO[Client](fetchDiscovery(_, issuer)).flatMap(remote(_, Expected(issuer, Some(audience))))
    )

  private def remote(jwksUri: String, expected: Expected): ZIO[Client & Rsa & Scope, OAuthError, JwtVerifier] =
    for
      client  <- ZIO.service[Client]
      rsa     <- ZIO.service[Rsa]
      cache   <- Ref.make(Option.empty[Jwks])
      fetched <- Ref.make(Instant.EPOCH)
      keys = KeySet(client, jwksUri, cache, fetched)
      _ <- keys.refresh.retry(Schedule.spaced(1.second) && Schedule.recurs(10))
      _ <- keys.refresh
        .catchAll(e => ZIO.logWarning(s"JWKS refresh from $jwksUri failed: ${e.message}"))
        .repeat(Schedule.spaced(5.minutes))
        .delay(5.minutes)
        .forkScoped
    yield Remote(keys, expected, rsa)

  private final case class Discovery(@jsonField("jwks_uri") jwksUri: String) derives JsonDecoder

  private def fetchDiscovery(client: Client, issuer: String): IO[OAuthError, String] =
    val url = issuer.stripSuffix("/") + "/.well-known/openid-configuration"
    fetch(client, url, ProviderEndpoint.Discovery).flatMap { json =>
      ZIO
        .fromEither(json.fromJson[Discovery])
        .mapBoth(OAuthError.BadResponse(ProviderEndpoint.Discovery, _), _.jwksUri)
    }

  private def fetch(client: Client, url: String, endpoint: ProviderEndpoint): IO[OAuthError, String] =
    client.batched(Request.get(url)).mapError(OAuthError.Transport(_)).flatMap { res =>
      res.body.utf8.mapError(OAuthError.Transport(_)).flatMap { body =>
        if res.status.isSuccess then ZIO.succeed(body) else ZIO.fail(OAuthError.Refused(endpoint, res.status, body))
      }
    }

  private def check(token: String, jwks: Jwks, expected: Expected, rsa: Rsa): IO[OAuthError, JwtClaim] =
    Jose.verify(token, jwks, expected).provideEnvironment(ZEnvironment(rsa)).mapError {
      case JoseError.Crypto(e) => OAuthError.Crypto(e)
      case e                   => OAuthError.InvalidToken(e)
    }

  private final class Static(jwks: Jwks, expected: Expected, rsa: Rsa) extends JwtVerifier:
    def verify(token: String): IO[OAuthError, JwtClaim] = check(token, jwks, expected, rsa)

  /** A cached key set. A token naming an unknown `kid` may mean the issuer rotated, so it refetches, but no more than
    * once every 30 seconds: otherwise every forged `kid` is an outbound request.
    */
  private final class KeySet(client: Client, uri: String, cache: Ref[Option[Jwks]], fetched: Ref[Instant]):
    val current: IO[OAuthError, Jwks] = cache.get.someOrFail(OAuthError.NoKeys)

    val refresh: IO[OAuthError, Unit] =
      fetch(client, uri, ProviderEndpoint.Jwks)
        .flatMap(json => ZIO.fromEither(Jose.parseJwks(json)).mapError(OAuthError.InvalidToken(_)))
        .flatMap(set => cache.set(Some(set)))
        .zipRight(Clock.instant.flatMap(fetched.set))

    val refreshIfStale: IO[OAuthError, Unit] =
      (Clock.instant <*> fetched.get).flatMap { (now, last) =>
        refresh.when(now.isAfter(last.plusSeconds(30))).unit
      }
  end KeySet

  private final class Remote(keys: KeySet, expected: Expected, rsa: Rsa) extends JwtVerifier:
    def verify(token: String): IO[OAuthError, JwtClaim] =
      keys.current.flatMap(check(token, _, expected, rsa)).catchSome {
        case OAuthError.InvalidToken(JoseError.NoMatchingKey) =>
          keys.refreshIfStale *> keys.current.flatMap(check(token, _, expected, rsa))
      }
end JwtVerifier
