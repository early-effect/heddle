package heddle.docs

import heddle.*
import heddle.crypto.Rsa
import heddle.oauth.jose.{Jose, SigningKey, TokenClaims}
import heddle.oauth.rs.JwtVerifier
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Auth extends DocSpecSuite:

  def doc = page("Auth")(
    md"""
HTTP primitives live in core. JWT verification lives in `heddle-oauth`. Missing credentials from
`Auth.*` are 401 with `WWW-Authenticate`.

On the example hub, writes and MCP HTTP share one bearer. Swagger Authorize and `POST /mcp` are
not two security stories.
""",
    section("Basic, Bearer, API key")(
      md"""
`Middleware.basicAuth` is the lock. `Auth.bearer` is the extractor you `provided` onto routes
that need a subject. API keys are `Auth.apiKey` / `Middleware.apiKey`.
""",
      exampleZIO {
        val locked =
          Routes(Method.GET / "secret" -> Handler.text("ok")) @@ Middleware.basicAuth("ada", "pw")
        val authed =
          Request.get("/secret").copy(headers = Headers.empty.set(Authorization.basic("ada", "pw")))
        for
          denied <- locked(Request.get("/secret"))
          ok     <- locked(authed)
          body   <- ok.body.utf8
        yield (denied.status, ok.status, body)
      }.assert { case (denied, ok, body) =>
        assertTrue(denied == Status.Unauthorized, ok == Status.Ok, body == "ok")
      },
    ),
    section("OAuth / OIDC")(
      md"""
`heddle-oauth` signs and verifies compact JWTs (RS256, our `Jose`), fetches JWKS, and speaks
authorization-code+PKCE, client credentials, refresh, device, and userinfo.

Signing and verifying are effects on the `Rsa` service (`Rsa.live` is the platform's own RSA: the
JCA, Node `crypto`, or OpenSSL). They read `Clock` for `iat` and `exp` and the CSPRNG for `jti`, so a
`TestClock` test can expire a token without waiting. A verifier checks the issuer and requires an
audience; a token whose header names no `kid` verifies only against a one-key set. A remote verifier
fetches its JWKS before its layer is built, refreshes every five minutes, and refetches for an
unknown `kid` at most once every 30 seconds, so forged `kid`s do not become outbound requests.
Failures are typed: `JoseError` inside `OAuthError.InvalidToken`, `RsaError` inside `OAuthError.Crypto`.

Stored passwords are salted PBKDF2-HMAC-SHA256 at 600,000 iterations, checked in constant time.

A loopback OpenID provider is `sbt oauth/run`
(`http://127.0.0.1:8080/.well-known/openid-configuration`).
`sbt example/run` embeds that OP next to the box office API so Swagger Authorize works against the
same process. Seed user `ada` / `ada`. Machine client `machine` / `secret`.
""",
      exampleZIO {
        val signedAndChecked =
          for
            key   <- SigningKey.generateRsa("k1")
            token <- Jose.sign(key, TokenClaims("ada", "http://iss", "api", Set("openid"), 5.minutes))
            claim <- JwtVerifier.static(key.publicJwksJson, "http://iss", "api").flatMap(_.verify(token))
          yield (claim.subject, claim.scopes.contains("openid"))
        signedAndChecked.provide(Rsa.live)
      }.assert { case (sub, openid) =>
        assertTrue(sub == "ada", openid)
      },
      md"""
`Endpoint.auth(SecurityScheme.OAuth2(...))` or `HttpBearer` is what Swagger uses for Authorize.
Do not put a second authorization server inside `heddle-mcp`. Resource-server JWT verify is the
adapter MCP HTTP already expects.
""",
    ),
  )
end Auth
