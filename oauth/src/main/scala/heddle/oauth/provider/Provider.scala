package heddle.oauth.provider

import heddle.http.{Form, Method, Request, Response, Status}
import heddle.http.header.{Authorization, AuthScheme, BasicCredentials, SetCookie}
import heddle.http.header.Authorization.given
import heddle.oauth.jose.{Jose, SigningKey}
import heddle.route.{Handler, Routes}
import heddle.route.PathDsl.*
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.Base64
import zio.{durationInt, Clock, UIO, ZIO}

final case class ProviderConfig(
    issuer: String,
    accessTokenTtl: zio.Duration = 15.minutes,
    refreshTokenTtl: zio.Duration = 8.hours,
    idTokenTtl: zio.Duration = 15.minutes,
    authorizationCodeTtl: zio.Duration = 2.minutes,
    sessionTtl: zio.Duration = 1.hour,
)

object Provider:
  def routes(config: ProviderConfig, stores: ProviderStores, key: SigningKey): Routes[Any, Nothing] =
    val iss = config.issuer.stripSuffix("/")
    Routes(
      Method.GET / ".well-known" / "openid-configuration" -> Handler { (_: Request) =>
        ZIO.succeed(Response.json(discovery(iss)))
      },
      Method.GET / "jwks.json"   -> Handler.json(key.publicJwksJson),
      Method.GET / "authorize"   -> Handler { (req: Request) => authorize(config, stores, req) },
      Method.GET / "login"       -> Handler { (req: Request) => loginGet(req) },
      Method.POST / "login"      -> Handler { (req: Request) => loginPost(config, stores, req) },
      Method.POST / "token"      -> Handler { (req: Request) => token(config, stores, key, req) },
      Method.GET / "userinfo"    -> Handler { (req: Request) => userinfo(config, stores, key, req) },
      Method.POST / "userinfo"   -> Handler { (req: Request) => userinfo(config, stores, key, req) },
      Method.POST / "introspect" -> Handler { (req: Request) => introspect(config, key, req) },
      Method.POST / "revoke"     -> Handler { (req: Request) => revoke(stores, req) },
    )
  end routes

  private def discovery(iss: String): String =
    s"""{"issuer":"$iss","authorization_endpoint":"$iss/authorize","token_endpoint":"$iss/token","userinfo_endpoint":"$iss/userinfo","jwks_uri":"$iss/jwks.json","introspection_endpoint":"$iss/introspect","revocation_endpoint":"$iss/revoke","response_types_supported":["code","code id_token"],"grant_types_supported":["authorization_code","client_credentials","refresh_token"],"subject_types_supported":["public"],"id_token_signing_alg_values_supported":["RS256"],"token_endpoint_auth_methods_supported":["client_secret_basic","client_secret_post","none"],"code_challenge_methods_supported":["S256"],"scopes_supported":["openid","profile","email","offline_access"]}"""

  private def authorize(config: ProviderConfig, stores: ProviderStores, req: Request): UIO[Response] =
    val q         = req.query
    val clientId  = q.get("client_id").getOrElse("")
    val redirect  = q.get("redirect_uri").getOrElse("")
    val state     = q.get("state")
    val scope     = q.get("scope").getOrElse("openid").split(" ").filter(_.nonEmpty).toSet
    val nonce     = q.get("nonce")
    val challenge = q.get("code_challenge")
    val method    = q.get("code_challenge_method")
    val resume    = req.url.render
    sessionUser(stores, req).flatMap {
      case None =>
        ZIO.succeed(Response.redirect("/login?resume=" + java.net.URLEncoder.encode(resume, StandardCharsets.UTF_8)))
      case Some(user) =>
        stores.clients.byId(clientId).flatMap {
          case None                                          => ZIO.succeed(Response.badRequest("unknown client"))
          case Some(c) if !c.redirectUris.contains(redirect) =>
            ZIO.succeed(Response.badRequest("redirect_uri mismatch"))
          case Some(_) if method.exists(_ != "S256") && challenge.isDefined =>
            ZIO.succeed(Response.badRequest("code_challenge_method must be S256"))
          case Some(_) =>
            val code = java.util.UUID.randomUUID().toString.replace("-", "")
            Clock.instant.flatMap { now =>
              stores.codes
                .put(
                  AuthCode(
                    code,
                    clientId,
                    user.id,
                    redirect,
                    scope,
                    nonce,
                    challenge,
                    now.plusMillis(config.authorizationCodeTtl.toMillis),
                  )
                )
                .as {
                  val loc = redirect + (if redirect.contains("?") then "&" else "?") +
                    s"code=$code" + state.map(s => s"&state=$s").getOrElse("")
                  Response.redirect(loc)
                }
            }
        }
    }
  end authorize

  private def loginGet(req: Request): UIO[Response] =
    val resume = req.query.get("resume").getOrElse("/")
    ZIO.succeed(
      Response.html(
        s"""<form method="post" action="/login">
           |<input type="hidden" name="resume" value="${escape(resume)}"/>
           |<label>user <input name="username"/></label>
           |<label>pass <input type="password" name="password"/></label>
           |<button type="submit">Sign in</button>
           |</form>""".stripMargin
      )
    )
  end loginGet

  private def loginPost(config: ProviderConfig, stores: ProviderStores, req: Request): UIO[Response] =
    req.body.asForm.orDie.flatMap { form =>
      val user   = form.get("username").getOrElse("")
      val pass   = form.get("password").getOrElse("")
      val resume = form.get("resume").getOrElse("/")
      stores.users.authenticate(user, pass).flatMap {
        case None    => ZIO.succeed(Response.text("invalid credentials", Status.Unauthorized))
        case Some(u) =>
          val sid = java.util.UUID.randomUUID().toString
          Clock.instant.flatMap { now =>
            stores.sessions
              .put(SessionRec(sid, u.id, now.plusMillis(config.sessionTtl.toMillis)))
              .as(
                Response
                  .redirect(resume)
                  .addCookie(
                    SetCookie(
                      "op_session",
                      sid,
                      httpOnly = true,
                      path = Some("/"),
                      sameSite = Some(heddle.http.header.SameSite.Lax),
                    )
                  )
              )
          }
      }
    }

  private def token(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      req: Request,
  ): UIO[Response] =
    req.body.asForm.orDie.flatMap { form =>
      clientOf(stores, req, form).flatMap {
        case None    => ZIO.succeed(Response.unauthorized("invalid_client"))
        case Some(c) =>
          form.get("grant_type") match
            case Some("authorization_code") => authCodeGrant(config, stores, key, c, form)
            case Some("client_credentials") => clientCredGrant(config, key, c, form)
            case Some("refresh_token")      => refreshGrant(config, stores, key, c, form)
            case other                      => ZIO.succeed(Response.badRequest(s"unsupported_grant_type: $other"))
      }
    }

  private def authCodeGrant(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      client: ClientRecord,
      form: Form,
  ): UIO[Response] =
    val code = form.get("code").getOrElse("")
    val uri  = form.get("redirect_uri").getOrElse("")
    val ver  = form.get("code_verifier")
    stores.codes.take(code).flatMap {
      case None     => ZIO.succeed(Response.badRequest("invalid_grant"))
      case Some(ac) =>
        Clock.instant.flatMap { now =>
          if ac.clientId != client.id || ac.redirectUri != uri || now.isAfter(ac.exp) then
            ZIO.succeed(Response.badRequest("invalid_grant"))
          else if ac.codeChallenge.exists(ch => !ver.exists(v => pkceOk(v, ch))) then
            ZIO.succeed(Response.badRequest("invalid_grant"))
          else issue(config, stores, key, client, ac.userId, ac.scopes, ac.nonce, now)
        }
    }
  end authCodeGrant

  private def clientCredGrant(
      config: ProviderConfig,
      key: SigningKey,
      client: ClientRecord,
      form: Form,
  ): UIO[Response] =
    if client.secretHash.isEmpty then ZIO.succeed(Response.unauthorized("confidential client required"))
    else
      val scope = form.get("scope").map(_.split(" ").filter(_.nonEmpty).toSet).getOrElse(Set.empty)
      val token = Jose.sign(key, client.id, config.issuer.stripSuffix("/"), client.id, scope, config.accessTokenTtl)
      ZIO.succeed(jsonToken(token, None, None, config.accessTokenTtl.toSeconds))

  private def refreshGrant(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      client: ClientRecord,
      form: Form,
  ): UIO[Response] =
    val tok = form.get("refresh_token").getOrElse("")
    stores.tokens.takeRefresh(tok).flatMap {
      case None                               => ZIO.succeed(Response.badRequest("invalid_grant"))
      case Some(r) if r.clientId != client.id => ZIO.succeed(Response.badRequest("invalid_grant"))
      case Some(r)                            =>
        Clock.instant.flatMap { now =>
          if now.isAfter(r.exp) then ZIO.succeed(Response.badRequest("invalid_grant"))
          else issue(config, stores, key, client, r.userId, r.scopes, None, now)
        }
    }
  end refreshGrant

  private def issue(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      client: ClientRecord,
      userId: String,
      scopes: Set[String],
      nonce: Option[String],
      now: Instant,
  ): UIO[Response] =
    val iss    = config.issuer.stripSuffix("/")
    val access = Jose.sign(key, userId, iss, client.id, scopes, config.accessTokenTtl)
    val idTok  =
      if scopes.contains("openid") then
        Some(Jose.sign(key, userId, iss, client.id, scopes, config.idTokenTtl, nonce.map("nonce" -> _).toMap))
      else None
    val refresh =
      if scopes.contains("offline_access") || scopes.contains("openid") then
        Some(java.util.UUID.randomUUID().toString.replace("-", ""))
      else None
    val put =
      refresh match
        case None     => ZIO.unit
        case Some(rt) =>
          stores.tokens.putRefresh(
            RefreshRec(rt, client.id, userId, scopes, rt, now.plusMillis(config.refreshTokenTtl.toMillis))
          )
    put.as(jsonToken(access, refresh, idTok, config.accessTokenTtl.toSeconds))
  end issue

  private def userinfo(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      req: Request,
  ): UIO[Response] =
    bearer(req) match
      case None      => ZIO.succeed(Response.unauthorized())
      case Some(tok) =>
        Jose.verify(tok, key.jwkSet, config.issuer.stripSuffix("/"), "") match
          case Left(_)  => ZIO.succeed(Response.unauthorized())
          case Right(c) =>
            stores.users.byId(c.subject).map {
              case None    => Response.notFound()
              case Some(u) =>
                val email = u.claims.getOrElse("email", s"${u.username}@example.test")
                Response.json(s"""{"sub":"${u.id}","preferred_username":"${u.username}","email":"$email"}""")
            }

  private def introspect(config: ProviderConfig, key: SigningKey, req: Request): UIO[Response] =
    req.body.asForm.orDie.map { form =>
      val tok = form.get("token").getOrElse("")
      Jose.verify(tok, key.jwkSet, config.issuer.stripSuffix("/"), "") match
        case Left(_)  => Response.json("""{"active":false}""")
        case Right(c) =>
          Response.json(s"""{"active":true,"sub":"${c.subject}","scope":"${c.scopes.mkString(" ")}"}""")
    }

  private def revoke(stores: ProviderStores, req: Request): UIO[Response] =
    req.body.asForm.orDie.flatMap { form =>
      val tok = form.get("token").getOrElse("")
      stores.tokens.takeRefresh(tok).as(Response.empty(Status.Ok))
    }

  private def clientOf(stores: ProviderStores, req: Request, form: Form): UIO[Option[ClientRecord]] =
    val basic  = req.headers.get[Authorization].flatMap(BasicCredentials.parse)
    val id     = basic.map(_.username).orElse(form.get("client_id"))
    val secret = basic.map(_.password).orElse(form.get("client_secret"))
    id match
      case None      => ZIO.succeed(None)
      case Some(cid) =>
        stores.clients.byId(cid).map {
          case None    => None
          case Some(c) =>
            c.secretHash match
              case None    => Some(c)
              case Some(h) => if secret.exists(Passwords.check(_, h)) then Some(c) else None
        }
    end match
  end clientOf

  private def sessionUser(stores: ProviderStores, req: Request): UIO[Option[UserRecord]] =
    req.cookie("op_session") match
      case None     => ZIO.succeed(None)
      case Some(id) =>
        stores.sessions.get(id).flatMap {
          case None    => ZIO.succeed(None)
          case Some(s) =>
            Clock.instant.flatMap { now =>
              if now.isAfter(s.exp) then ZIO.succeed(None) else stores.users.byId(s.userId)
            }
        }

  private def pkceOk(verifier: String, challenge: String): Boolean =
    val digest = MessageDigest.getInstance("SHA-256").digest(verifier.getBytes(StandardCharsets.US_ASCII))
    Base64.getUrlEncoder.withoutPadding.encodeToString(digest) == challenge

  private def bearer(req: Request): Option[String] =
    req.headers.get[Authorization] match
      case Some(Authorization(AuthScheme.Bearer, t)) if t.nonEmpty => Some(t)
      case _                                                       => None

  private def jsonToken(access: String, refresh: Option[String], idToken: Option[String], expires: Long): Response =
    val extra =
      refresh.map(r => s""","refresh_token":"$r"""").getOrElse("") +
        idToken.map(t => s""","id_token":"$t"""").getOrElse("")
    Response.json(s"""{"access_token":"$access","token_type":"Bearer","expires_in":$expires$extra}""")

  private def escape(s: String): String =
    s.replace("&", "&amp;").replace("\"", "&quot;").replace("<", "&lt;")
end Provider
