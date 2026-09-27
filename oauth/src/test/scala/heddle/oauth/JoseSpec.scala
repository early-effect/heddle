package heddle.oauth

import heddle.oauth.jose.{Jose, JoseError, SigningKey}
import zio.*
import zio.test.*

object JoseSpec extends ZIOSpecDefault:
  def spec =
    suite("Jose")(
      test("sign and verify a compact RS256 JWT"):
        ZIO.serviceWith[SigningKey] { key =>
          val issuer = "http://iss"
          val aud    = "api"
          val token  = Jose.sign(key, "ada", issuer, aud, Set("openid", "profile"), 5.minutes)
          val claim  = Jose.verify(token, key.jwks, issuer, aud)
          assertTrue(
            token.split("\\.").length == 3,
            claim.exists(_.subject == "ada"),
            claim.exists(_.scopes.contains("openid")),
            claim.exists(_.audience.contains(aud)),
            claim.exists(_.jwtId.nonEmpty),
          )
        }
      ,
      test("parseJwks reads the public set we emit"):
        ZIO.serviceWith[SigningKey] { key =>
          Jose.parseJwks(key.publicJwksJson) match
            case Left(err)   => assertNever(err.message)
            case Right(jwks) => assertTrue(jwks.keys.map(k => (k.kid, k.n.nonEmpty)) == List((key.kid, true)))
        }
      ,
      test("verify rejects an expired token"):
        ZIO.serviceWith[SigningKey] { key =>
          val token = Jose.sign(key, "ada", "http://iss", "api", Set.empty, (-5).seconds)
          assertTrue(Jose.verify(token, key.jwks, "http://iss", "api") == Left(JoseError.Expired))
        }
      ,
      test("verify rejects alg none"):
        ZIO.serviceWith[SigningKey] { key =>
          val none = "eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZGEifQ.e30"
          assertTrue(Jose.verify(none, key.jwks, "http://iss", "api") == Left(JoseError.UnsupportedAlg("none")))
        }
      ,
      test("verify rejects a bad issuer"):
        ZIO.serviceWith[SigningKey] { key =>
          val token = Jose.sign(key, "ada", "http://iss", "api", Set.empty, 5.minutes)
          assertTrue(
            Jose.verify(token, key.jwks, "http://other", "api") == Left(
              JoseError.IssuerMismatch("http://other", "http://iss")
            )
          )
        },
    ).provideShared(TestKeys.signing("k1")) @@ TestAspect.timeout(10.seconds)
end JoseSpec
