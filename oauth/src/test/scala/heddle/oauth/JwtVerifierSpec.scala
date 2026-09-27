package heddle.oauth

import heddle.client.{Client, ClientError, TargetError}
import heddle.crypto.Rsa
import heddle.http.{Request, Response}
import heddle.oauth.client.{AuthzRequest, OAuthClient, OAuthEndpoints, Registration}
import heddle.oauth.jose.{Jose, JoseError, SigningKey, TokenClaims}
import heddle.oauth.rs.JwtVerifier
import zio.*
import zio.test.*

object JwtVerifierSpec extends ZIOSpecDefault:
  private def claims() = TokenClaims("ada", "http://iss", "api", Set("openid"), 5.minutes)

  private val offline: Client = new Client:
    def batched(req: Request)   = ZIO.fail(ClientError.InvalidTarget(req.url.render, TargetError.NoHost))
    def streaming(req: Request) = batched(req)

  /** A client that serves one body at every URL and counts its requests. */
  private def serving(body: Ref[String], hits: Ref[Int]): Client = new Client:
    def batched(req: Request)   = hits.update(_ + 1) *> body.get.map(Response.json(_))
    def streaming(req: Request) = batched(req)

  def spec =
    suite("JwtVerifier")(
      test("a static verifier checks issuer and audience"):
        for
          key   <- ZIO.service[SigningKey]
          token <- Jose.sign(key, claims())
          ok    <- JwtVerifier.static(key.publicJwksJson, "http://iss", "api").flatMap(_.verify(token))
          iss   <- JwtVerifier.static(key.publicJwksJson, "http://other", "api").flatMap(_.verify(token)).either
          aud   <- JwtVerifier.static(key.publicJwksJson, "http://iss", "web").flatMap(_.verify(token)).either
        yield assertTrue(
          ok.subject == "ada",
          iss == Left(OAuthError.InvalidToken(JoseError.IssuerMismatch("http://other", "http://iss"))),
          aud == Left(OAuthError.InvalidToken(JoseError.AudienceMismatch("web", List("api")))),
        )
      ,
      test("a remote verifier refetches for an unknown kid, at most once every 30 seconds, and never for an expiry"):
        for
          old     <- ZIO.service[SigningKey]
          rotated <- SigningKey.generateRsa("k2")
          body    <- Ref.make(old.publicJwksJson)
          hits    <- Ref.make(0)
          result  <- ZIO.scoped {
            JwtVerifier
              .jwks("http://iss/jwks", "http://iss", "api")
              .build
              .provideSomeLayer[Rsa & Scope](ZLayer.succeed(serving(body, hits)))
              .map(_.get)
              .flatMap { v =>
                for
                  expired  <- Jose.sign(old, claims().copy(ttl = (-1).second))
                  _        <- v.verify(expired).either
                  afterExp <- hits.get
                  _        <- body.set(rotated.publicJwksJson)
                  _        <- TestClock.adjust(31.seconds)
                  fresh    <- Jose.sign(rotated, claims())
                  ok       <- v.verify(fresh).either
                  afterRot <- hits.get
                  forged   <- Jose.sign(SigningKey(old.key.hashCode.toString, old.key), claims())
                  _        <- ZIO.foreachDiscard(1 to 5)(_ => v.verify(forged).either)
                  afterBad <- hits.get
                yield (afterExp, ok.isRight, afterRot, afterBad)
              }
          }
          (afterExp, rotatedOk, afterRot, afterBad) = result
        yield assertTrue(afterExp == 1, rotatedOk, afterRot == 2, afterBad == 2)
      ,
      test("a remote verifier whose keys never load fails to build, typed"):
        val build = ZIO.scoped(
          JwtVerifier
            .jwks("http://iss/jwks", "http://iss", "api")
            .build
            .provideSomeLayer[Rsa & Scope](ZLayer.succeed(offline))
        )
        for
          fiber <- build.either.fork
          _     <- TestClock.adjust(11.seconds)
          out   <- fiber.join
        yield assertTrue(out.left.exists {
          case OAuthError.Transport(_: ClientError.InvalidTarget) => true
          case _                                                  => false
        })
      ,
      test("authorizationUrl carries the S256 challenge, and PKCE verifiers are 43 characters of fresh entropy"):
        val oc = OAuthClient(offline, Registration("cid"), OAuthEndpoints("http://iss/authorize", "http://iss/token"))
        (OAuthClient.pkce <*> OAuthClient.pkce).flatMap { (a, b) =>
          oc.authorizationUrl(AuthzRequest("http://app/cb", Set("openid"), "st"), a).map { url =>
            assertTrue(
              url.contains(s"code_challenge=${a.challenge}"),
              url.contains("code_challenge_method=S256"),
              url.contains("client_id=cid"),
              a.verifier.length == 43,
              a.verifier != b.verifier,
            )
          }
        },
    ).provideShared(TestKeys.signing("k1")) @@ TestAspect.timeout(60.seconds)
end JwtVerifierSpec
