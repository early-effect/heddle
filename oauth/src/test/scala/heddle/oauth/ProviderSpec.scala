package heddle.oauth

import heddle.*
import heddle.oauth.client.{AuthzRequest, OAuthClient}
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
            val json = res.body.asString
            assertTrue(
              json.contains("authorization_endpoint"),
              json.contains("code_challenge_methods_supported"),
              json.contains("S256"),
              json.contains("token_endpoint"),
            )
          }
        }
      ,
      test("authorization code + PKCE issues a JWT"):
        withOp { (base, key, _) =>
          val pkce = OAuthClient.pkce()
          val http = new Client:
            def batched(req: Request) = Client.request(req.method, abs(base, req), req.headers, req.body)
          val oc   = OAuthClient(http, base, "web", None, s"$base/authorize", s"$base/token", Some(s"$base/userinfo"))
          val auth = AuthzRequest(s"$base/cb", Set("openid", "profile"), "st")
          for
            authz <- oc.authorizationUrl(auth, pkce)
            login <- Client.request(
              Method.POST,
              s"$base/login",
              body = Body.form(Form("username" -> "ada", "password" -> "ada", "resume" -> authz)),
            )
            cookie = login.headers.setCookies.headOption.map(_.value).getOrElse("")
            loc1   = login.header("Location").getOrElse(authz)
            step3 <- Client.request(
              Method.GET,
              abs(base, Request.get(loc1).addCookie("op_session", cookie)),
              Request.get(loc1).addCookie("op_session", cookie).headers,
            )
            loc  = step3.header("Location").getOrElse("")
            code = queryParam(loc, "code").getOrElse("")
            tokens <- oc.exchange(code, s"$base/cb", pkce)
            claim  <- JwtVerifier.static(key.publicJwksJson, base, "web").flatMap(_.verify(tokens.accessToken))
          yield assertTrue(
            login.status == Status.Found,
            code.nonEmpty,
            tokens.accessToken.nonEmpty,
            claim.subject == "u1",
            claim.scopes.contains("openid"),
          )
          end for
        }
      ,
      test("client credentials grant"):
        withOp { (base, key, _) =>
          val http = new Client:
            def batched(req: Request) = Client.request(req.method, abs(base, req), req.headers, req.body)
          val oc = OAuthClient(http, base, "machine", Some("secret"), s"$base/authorize", s"$base/token")
          for
            tokens <- oc.clientCredentials(Set("api"))
            claim  <- JwtVerifier.static(key.publicJwksJson, base, "machine").flatMap(_.verify(tokens.accessToken))
          yield assertTrue(tokens.tokenType == "Bearer", claim.subject == "machine")
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

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

  private def withOp[E, A](f: (String, SigningKey, ProviderStores) => IO[E, A]): ZIO[Any, Any, A] =
    ZIO.scoped {
      for
        _ <- MemoryStores.seed(
          List(UserRecord("u1", "ada", Passwords.hash("ada"))),
          List(
            ClientRecord("web", None, List("http://127.0.0.1/cb", "http://127.0.0.1:0/cb"), Set("authorization_code")),
            ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
          ),
        )
        key = SigningKey.generateRsa("op")
        port <- ZIO.attemptBlocking {
          val s = new java.net.ServerSocket(0, 0, java.net.InetAddress.getByName("127.0.0.1"))
          val p = s.getLocalPort
          s.close()
          p
        }
        base   = s"http://127.0.0.1:$port"
        seeded = List(
          ClientRecord("web", None, List(s"$base/cb"), Set("authorization_code")),
          ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
        )
        stores2 <- MemoryStores.seed(List(UserRecord("u1", "ada", Passwords.hash("ada"))), seeded)
        _       <- Server.install(
          Provider.routes(ProviderConfig(base), stores2, key),
          Server.Config.default.copy(host = "127.0.0.1", port = port),
        )
        a <- f(base, key, stores2)
      yield a
    }
end ProviderSpec
