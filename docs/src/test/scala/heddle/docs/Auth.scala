package heddle.docs

import heddle.*
import heddle.oauth.jose.{Jose, SigningKey}
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
        yield (denied.status, ok.status, ok.body.asString)
      }.assert { case (denied, ok, body) =>
        assertTrue(denied == Status.Unauthorized, ok == Status.Ok, body == "ok")
      },
    ),
    section("OAuth / OIDC")(
      md"""
`heddle-oauth` signs and verifies JWTs (Nimbus under a Scala API), fetches JWKS, and speaks
authorization-code+PKCE, client credentials, refresh, device, and userinfo.

A loopback OpenID provider is `sbt oauth/run`
(`http://127.0.0.1:8080/.well-known/openid-configuration`).
`sbt example/run` embeds that OP next to the directory API so Swagger Authorize works against the
same process. Seed user `ada` / `ada`. Machine client `machine` / `secret`.
""",
      exampleZIO {
        val key    = SigningKey.generateRsa("k1")
        val issuer = "http://iss"
        val aud    = "api"
        val token  = Jose.sign(key, "ada", issuer, aud, Set("openid"), 5.minutes)
        JwtVerifier.static(key.publicJwksJson, issuer, aud).flatMap(_.verify(token)).map { claim =>
          (claim.subject, claim.scopes.contains("openid"))
        }
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
