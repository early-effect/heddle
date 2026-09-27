package heddle.oauth

import heddle.*
import heddle.crypto.Rsa
import heddle.oauth.client.{AuthzRequest, OAuthClient, OAuthEndpoints, Registration}
import heddle.oauth.jose.SigningKey
import heddle.oauth.provider.*
import heddle.oauth.rs.JwtVerifier
import zio.*
import zio.test.*

object ProviderSpec extends ZIOSpecDefault:
  def spec =
    suite("OIDC provider")(
      test("discovery advertises code PKCE and token endpoint"):
        withOp { (base, _, _) =>
          Client.get(s"$base/.well-known/openid-configuration").map { res =>
            val json = res.body.text
            assertTrue(
              json.is(_.some).contains("authorization_endpoint"),
              json.is(_.some).contains("code_challenge_methods_supported"),
              json.is(_.some).contains("S256"),
              json.is(_.some).contains("token_endpoint"),
            )
          }
        }
      ,
      test("authorization code + PKCE issues a JWT"):
        withOp { (base, key, _) =>
          val oc   = OAuthClient(local(base), Registration("web"), endpoints(base))
          val auth = AuthzRequest("http://127.0.0.1/cb", Set("openid", "profile"), "a b&c")
          for
            pkce  <- OAuthClient.pkce
            authz <- oc.authorizationUrl(auth, pkce)
            login <- Client.request(
              Method.POST,
              s"$base/login",
              body = Body.form(Form("username" -> "ada", "password" -> "ada", "resume" -> localPart(authz))),
            )
            cookie = login.headers.setCookies.headOption.map(_.value).getOrElse("")
            loc1   = login.header("Location").getOrElse("")
            step3 <- Client.request(
              Method.GET,
              abs(base, Request.get(loc1).addCookie("op_session", cookie)),
              Request.get(loc1).addCookie("op_session", cookie).headers,
            )
            loc  = step3.header("Location").getOrElse("")
            code = queryParam(loc, "code").getOrElse("")
            tokens <- oc.exchange(code, "http://127.0.0.1/cb", pkce)
            claim  <- JwtVerifier
              .static(key.publicJwksJson, "http://127.0.0.1", "web")
              .flatMap(_.verify(tokens.accessToken))
          yield assertTrue(
            login.status == Status.Found,
            loc1 == localPart(authz),
            code.length == 32,
            queryParam(loc, "state").contains("a b&c"),
            tokens.idToken.nonEmpty,
            tokens.refreshToken.nonEmpty,
            claim.subject == "u1",
            claim.scopes.contains("openid"),
          )
          end for
        }
      ,
      test("client credentials grant, and a wrong secret is refused with the server's own error"):
        withOp { (base, key, _) =>
          val oc    = OAuthClient(local(base), Registration("machine", Some("secret")), endpoints(base))
          val wrong = OAuthClient(local(base), Registration("machine", Some("guess")), endpoints(base))
          for
            tokens <- oc.clientCredentials(Set("api"))
            claim  <- JwtVerifier
              .static(key.publicJwksJson, "http://127.0.0.1", "machine")
              .flatMap(_.verify(tokens.accessToken))
            refused <- wrong.clientCredentials(Set("api")).either
          yield assertTrue(
            tokens.tokenType == "Bearer",
            claim.subject == "machine",
            refused.left.exists {
              case OAuthError.Refused(ProviderEndpoint.Token, Status.Unauthorized, _) => true
              case _                                                                  => false
            },
          )
          end for
        }
      ,
      test("login refuses to resume anywhere but this server"):
        withOp { (base, _, _) =>
          ZIO
            .foreach(List("https://evil.example/", "//evil.example/", "/\\evil.example"))(resume =>
              Client.request(
                Method.POST,
                s"$base/login",
                body = Body.form(Form("username" -> "ada", "password" -> "ada", "resume" -> resume)),
              )
            )
            .map(rs => assertTrue(rs.forall(_.header("Location").contains("/"))))
        }
      ,
      test("a public client must send PKCE"):
        withOp { (base, _, _) =>
          for
            login <- Client.request(
              Method.POST,
              s"$base/login",
              body = Body.form(Form("username" -> "ada", "password" -> "ada", "resume" -> "/")),
            )
            cookie = login.headers.setCookies.headOption.map(_.value).getOrElse("")
            path   = "/authorize?client_id=web&redirect_uri=http%3A%2F%2F127.0.0.1%2Fcb&scope=openid"
            res <- Client.request(Method.GET, s"$base$path", Request.get(path).addCookie("op_session", cookie).headers)
          yield assertTrue(res.status == Status.BadRequest)
        }
      ,
      test("passwords are salted, and check rejects a wrong or malformed hash"):
        for
          a <- Passwords.hash("ada", iterations = 1000)
          b <- Passwords.hash("ada", iterations = 1000)
        yield assertTrue(
          a != b,
          Passwords.check("ada", a),
          !Passwords.check("adb", a),
          !Passwords.check("ada", "sha256:abc"),
          !Passwords.check("ada", "pbkdf2-sha256$0$AAAA$AAAA"),
        ),
    ).provideShared(TestKeys.signing("op")) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds) @@
      TestAspect.withLiveClock

  private def endpoints(base: String) = OAuthEndpoints(s"$base/authorize", s"$base/token", Some(s"$base/userinfo"))

  /** Sends relative requests to `base`, the way a browser follows the provider's redirects. */
  private def local(base: String): Client = new Client:
    def batched(req: Request)   = Client.request(req.method, abs(base, req), req.headers, req.body)
    def streaming(req: Request) = batched(req)

  private def localPart(url: String): String =
    val scheme = url.indexOf("://")
    val path   = if scheme < 0 then -1 else url.indexOf('/', scheme + 3)
    if path < 0 then url else url.substring(path)

  private def abs(base: String, req: Request): String =
    val p = req.url.render
    if p.startsWith("http") then p else base.stripSuffix("/") + (if p.startsWith("/") then p else "/" + p)

  private def queryParam(url: String, name: String): Option[String] =
    val q = url.indexOf('?')
    if q < 0 then None
    else
      url
        .substring(q + 1)
        .split('&')
        .toList
        .map(_.split("=", 2))
        .collectFirst { case Array(`name`, v) => java.net.URLDecoder.decode(v, "UTF-8") }
  end queryParam

  private def withOp[E, A](f: (String, SigningKey, ProviderStores) => ZIO[Rsa, E, A]): ZIO[SigningKey & Rsa, Any, A] =
    val issuer = "http://127.0.0.1"
    ZIO.scoped {
      for
        key    <- ZIO.service[SigningKey]
        ada    <- Passwords.hash("ada", iterations = 1000)
        secret <- Passwords.hash("secret", iterations = 1000)
        stores <- MemoryStores.seed(
          List(UserRecord("u1", "ada", ada)),
          List(
            ClientRecord("web", None, List("http://127.0.0.1/cb"), Set("authorization_code")),
            ClientRecord("machine", Some(secret), Nil, Set("client_credentials")),
          ),
        )
        server <- Server.install(
          Provider.routes(ProviderConfig(issuer), stores, key),
          Server.Config.default.copy(host = "127.0.0.1", port = 0),
        )
        port <- server.port
        a    <- f(s"$issuer:$port", key, stores)
      yield a
    }
  end withOp
end ProviderSpec
