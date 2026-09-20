package heddle.oauth

import heddle.oauth.jose.{Jose, SigningKey}
import zio.*
import zio.test.*

object JoseSpec extends ZIOSpecDefault:
  def spec =
    suite("Jose")(
      test("sign and verify a compact RS256 JWT"):
        val key    = SigningKey.generateRsa("k1")
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
      ,
      test("parseJwks reads the public set we emit"):
        val key = SigningKey.generateRsa("op")
        Jose.parseJwks(key.publicJwksJson) match
          case Left(err)   => assertTrue(err.isEmpty)
          case Right(jwks) => assertTrue(jwks.keys.map(_.kid) == List("op"), jwks.keys.head.n.nonEmpty)
      ,
      test("verify rejects an expired token"):
        val key   = SigningKey.generateRsa("k1")
        val token = Jose.sign(key, "ada", "http://iss", "api", Set.empty, (-5).seconds)
        assertTrue(Jose.verify(token, key.jwks, "http://iss", "api") == Left("expired"))
      ,
      test("verify rejects alg none"):
        val key  = SigningKey.generateRsa("k1")
        val none = "eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZGEifQ.e30"
        val got  = Jose.verify(none, key.jwks, "http://iss", "api")
        assertTrue(got == Left("unsupported alg: none"))
      ,
      test("verify rejects a bad issuer"):
        val key   = SigningKey.generateRsa("k1")
        val token = Jose.sign(key, "ada", "http://iss", "api", Set.empty, 5.minutes)
        assertTrue(Jose.verify(token, key.jwks, "http://other", "api") == Left("issuer mismatch")),
    ) @@ TestAspect.timeout(10.seconds)
end JoseSpec
