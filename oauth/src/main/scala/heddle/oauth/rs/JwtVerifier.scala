package heddle.oauth.rs

import com.nimbusds.jose.jwk.JWKSet
import heddle.client.Client
import heddle.http.Request
import heddle.oauth.OAuthError
import heddle.oauth.jose.Jose
import zio.{IO, Ref, Schedule, ZIO, ZLayer, durationInt}

trait JwtVerifier:
  def verify(token: String): IO[OAuthError, JwtClaim]

object JwtVerifier:
  def static(jwksJson: String, issuer: String, audience: String): IO[OAuthError, JwtVerifier] =
    ZIO.fromEither(Jose.parseJwks(jwksJson)).mapError(OAuthError.InvalidToken.apply).map { jwks =>
      Static(jwks, issuer, audience)
    }

  def staticLayer(jwksJson: String, issuer: String, audience: String): ZLayer[Any, OAuthError, JwtVerifier] =
    ZLayer.fromZIO(static(jwksJson, issuer, audience))

  def jwks(jwksUri: String, issuer: String, audience: String): ZLayer[Client, Nothing, JwtVerifier] =
    ZLayer.scoped {
      for
        client <- ZIO.service[Client]
        cache  <- Ref.make(Option.empty[JWKSet])
        _      <- refresh(client, jwksUri, cache)
          .mapError(e => RuntimeException(e.message))
          .retry(Schedule.spaced(1.second).upTo(10.seconds))
          .orDie
        _ <- refresh(client, jwksUri, cache)
          .mapError(e => RuntimeException(e.message))
          .repeat(Schedule.spaced(5.minutes))
          .forkScoped
          .unit
      yield Remote(client, jwksUri, issuer, audience, cache)
    }

  def issuer(issuer: String, audience: String): ZLayer[Client, Nothing, JwtVerifier] =
    ZLayer.scoped {
      for
        client <- ZIO.service[Client]
        disc   <- fetchDiscovery(client, issuer).mapError(e => RuntimeException(e.message)).orDie
        jwksUri = disc
        cache <- Ref.make(Option.empty[JWKSet])
        _     <- refresh(client, jwksUri, cache).mapError(e => RuntimeException(e.message)).orDie
      yield Remote(client, jwksUri, issuer, audience, cache)
    }

  private def fetchDiscovery(client: Client, issuer: String): IO[OAuthError, String] =
    val url = issuer.stripSuffix("/") + "/.well-known/openid-configuration"
    client
      .batched(Request.get(url))
      .mapError(OAuthError.Transport.apply)
      .flatMap { res =>
        res.body.utf8.mapError(OAuthError.Transport.apply).flatMap { json =>
          val key = "\"jwks_uri\""
          val i   = json.indexOf(key)
          if i < 0 then ZIO.fail(OAuthError.Discovery("jwks_uri missing"))
          else
            val from = json.indexOf('"', i + key.length)
            val to   = json.indexOf('"', from + 1)
            if from < 0 || to < 0 then ZIO.fail(OAuthError.Discovery("jwks_uri missing"))
            else ZIO.succeed(json.substring(from + 1, to))
        }
      }
  end fetchDiscovery

  private def refresh(client: Client, jwksUri: String, cache: Ref[Option[JWKSet]]): IO[OAuthError, Unit] =
    client
      .batched(Request.get(jwksUri))
      .mapError(OAuthError.Transport.apply)
      .flatMap { res =>
        res.body.utf8.mapError(OAuthError.Transport.apply).flatMap { json =>
          ZIO.fromEither(Jose.parseJwks(json)).mapError(OAuthError.InvalidToken.apply).flatMap { set =>
            cache.set(Some(set))
          }
        }
      }

  private final class Static(jwks: JWKSet, issuer: String, audience: String) extends JwtVerifier:
    def verify(token: String): IO[OAuthError, JwtClaim] =
      ZIO.fromEither(Jose.verify(token, jwks, issuer, audience)).mapError(OAuthError.InvalidToken.apply)

  private final class Remote(
      client: Client,
      jwksUri: String,
      issuer: String,
      audience: String,
      cache: Ref[Option[JWKSet]],
  ) extends JwtVerifier:
    def verify(token: String): IO[OAuthError, JwtClaim] =
      cache.get.flatMap {
        case None       => ZIO.fail(OAuthError.InvalidToken("JWKS not loaded"))
        case Some(jwks) =>
          Jose.verify(token, jwks, issuer, audience) match
            case Right(c) => ZIO.succeed(c)
            case Left(_)  =>
              refresh(client, jwksUri, cache) *>
                cache.get.flatMap {
                  case None        => ZIO.fail(OAuthError.InvalidToken("JWKS not loaded"))
                  case Some(jwks2) =>
                    ZIO.fromEither(Jose.verify(token, jwks2, issuer, audience)).mapError(OAuthError.InvalidToken.apply)
                }
      }
  end Remote
end JwtVerifier
