package heddle.oauth

import heddle.crypto.{Base64Url, Rsa}
import heddle.oauth.jose.{Expected, Jose, JoseError, Jwks, SigningKey, TokenClaims}
import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.*
import zio.test.*

object JoseSpec extends ZIOSpecDefault:
  private val api = Expected("http://iss", Some("api"))

  private def claims(ttl: Duration = 5.minutes) = TokenClaims("ada", "http://iss", "api", Set("openid"), ttl)

  /** A token signed by `key` over a payload we choose, for claims `Jose.sign` would never produce. */
  private def raw(key: SigningKey, header: String, payload: String): ZIO[Rsa, Any, String] =
    val b64   = (s: String) => Base64Url.encode(Chunk.fromArray(s.getBytes(StandardCharsets.UTF_8)))
    val input = s"${b64(header)}.${b64(payload)}"
    Rsa
      .signSha256(key.key, Chunk.fromArray(input.getBytes(StandardCharsets.UTF_8)))
      .map(s => s"$input.${Base64Url.encode(s)}")

  def spec =
    suite("Jose")(
      test("sign then verify returns the claims, with iat and jti from the clock and CSPRNG"):
        for
          key   <- ZIO.service[SigningKey]
          _     <- TestClock.setTime(Instant.ofEpochSecond(1_000_000))
          token <- Jose.sign(key, claims())
          claim <- Jose.verify(token, key.jwks, api)
        yield assertTrue(
          claim.subject == "ada",
          claim.scopes == Set("openid"),
          claim.audience == List("api"),
          claim.issuedAt.contains(Instant.ofEpochSecond(1_000_000)),
          claim.expiresAt == Instant.ofEpochSecond(1_000_300),
          claim.jwtId.exists(_.nonEmpty),
        )
      ,
      test("a token expires when the clock passes exp"):
        for
          key   <- ZIO.service[SigningKey]
          token <- Jose.sign(key, claims())
          fresh <- Jose.verify(token, key.jwks, api).either
          _     <- TestClock.adjust(5.minutes + 1.second)
          stale <- Jose.verify(token, key.jwks, api).either
        yield assertTrue(fresh.isRight, stale == Left(JoseError.Expired))
      ,
      test("issuer and audience must match, and no audience means any"):
        for
          key   <- ZIO.service[SigningKey]
          token <- Jose.sign(key, claims())
          iss   <- Jose.verify(token, key.jwks, Expected("http://other", Some("api"))).either
          aud   <- Jose.verify(token, key.jwks, Expected("http://iss", Some("web"))).either
          any   <- Jose.verify(token, key.jwks, Expected("http://iss", None)).either
        yield assertTrue(
          iss == Left(JoseError.IssuerMismatch("http://other", "http://iss")),
          aud == Left(JoseError.AudienceMismatch("web", List("api"))),
          any.isRight,
        )
      ,
      test("the header's kid picks the key; with no kid only a one-key set answers"):
        for
          key   <- ZIO.service[SigningKey]
          other <- SigningKey.generateRsa("k2")
          two = Jwks(List(other.publicJwk, key.publicJwk))
          named   <- Jose.sign(key, claims())
          ok      <- Jose.verify(named, two, api).either
          unknown <- Jose.verify(named, other.jwks, api).either
          bare  <- raw(key, """{"alg":"RS256"}""", """{"sub":"ada","iss":"http://iss","aud":"api","exp":9999999999}""")
          alone <- Jose.verify(bare, key.jwks, api).either
          among <- Jose.verify(bare, two, api).either
        yield assertTrue(
          ok.isRight,
          unknown == Left(JoseError.NoMatchingKey),
          alone.isRight,
          among == Left(JoseError.KeyUnnamed),
        )
      ,
      test("sub and exp are required"):
        for
          key   <- ZIO.service[SigningKey]
          noSub <- raw(key, """{"alg":"RS256","kid":"k1"}""", """{"iss":"http://iss","aud":"api","exp":9999999999}""")
          noExp <- raw(key, """{"alg":"RS256","kid":"k1"}""", """{"sub":"ada","iss":"http://iss","aud":"api"}""")
          a     <- Jose.verify(noSub, key.jwks, api).either
          b     <- Jose.verify(noExp, key.jwks, api).either
        yield assertTrue(a == Left(JoseError.MissingClaim("sub")), b == Left(JoseError.MissingClaim("exp")))
      ,
      test("a tampered payload does not verify, and alg none is refused"):
        for
          key   <- ZIO.service[SigningKey]
          token <- Jose.sign(key, claims())
          root   = Base64Url.encode(Chunk.fromArray("""{"sub":"root"}""".getBytes))
          forged = token.split("\\.", -1) match
            case Array(h, _, s) => s"$h.$root.$s"
            case _              => token
          bad  <- Jose.verify(forged, key.jwks, api).either
          none <- Jose.verify("eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZGEifQ.e30", key.jwks, api).either
        yield assertTrue(bad == Left(JoseError.BadSignature), none == Left(JoseError.UnsupportedAlg("none")))
      ,
      test("parseJwks reads the public set we emit"):
        ZIO.serviceWith[SigningKey] { key =>
          assertTrue(
            Jose.parseJwks(key.publicJwksJson).map(_.keys.map(k => (k.kid, k.public))) == Right(
              List((key.kid, key.key.public))
            )
          )
        },
    ).provideShared(TestKeys.signing("k1")) @@ TestAspect.timeout(60.seconds)
end JoseSpec
