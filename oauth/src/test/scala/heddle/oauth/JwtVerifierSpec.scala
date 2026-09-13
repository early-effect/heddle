package heddle.oauth

import heddle.oauth.jose.{Jose, SigningKey}
import heddle.oauth.rs.JwtVerifier
import zio.*
import zio.test.*

object JwtVerifierSpec extends ZIOSpecDefault:
  def spec =
    suite("JwtVerifier")(
      test("static verifier accepts a signed access token"):
        val key    = SigningKey.generateRsa("k1")
        val issuer = "http://iss"
        val aud    = "api"
        val token  = Jose.sign(key, "ada", issuer, aud, Set("openid", "profile"), 5.minutes)
        JwtVerifier.static(key.publicJwksJson, issuer, aud).flatMap(_.verify(token)).map { claim =>
          assertTrue(claim.subject == "ada", claim.scopes.contains("openid"), claim.audience.contains(aud))
        }
      ,
      test("static verifier rejects a bad issuer"):
        val key   = SigningKey.generateRsa("k1")
        val token = Jose.sign(key, "ada", "http://iss", "api", Set.empty, 5.minutes)
        JwtVerifier.static(key.publicJwksJson, "http://other", "api").flatMap { v =>
          v.verify(token).either.map(e => assertTrue(e.isLeft))
        }
      ,
      test("authorizationUrl includes PKCE S256"):
        val dummy = new heddle.client.Client:
          def batched(req: heddle.http.Request) = ZIO.fail(new RuntimeException("unused"))
        val oc =
          heddle.oauth.client.OAuthClient(dummy, "http://iss", "cid", None, "http://iss/authorize", "http://iss/token")
        val pkce = heddle.oauth.client.OAuthClient.pkce()
        oc.authorizationUrl(
          heddle.oauth.client.AuthzRequest("http://app/cb", Set("openid"), "st"),
          pkce,
        ).map { url =>
          assertTrue(
            url.contains("code_challenge="),
            url.contains("code_challenge_method=S256"),
            url.contains("client_id=cid"),
            pkce.challenge.nonEmpty,
          )
        },
    ) @@ TestAspect.timeout(10.seconds)
end JwtVerifierSpec
