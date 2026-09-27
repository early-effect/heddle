package heddle.oauth.provider

import heddle.crypto.{Base64Url, DigestPlatform, Rsa, RsaError}
import heddle.http.{Form, Method, Request, Response, Status}
import heddle.http.header.{Authorization, AuthScheme, BasicCredentials, SetCookie}
import heddle.http.header.Authorization.given
import heddle.internal.Ids
import heddle.oauth.jose.{Expected, Jose, SigningKey, TokenClaims}
import heddle.route.{Handler, Routes}
import heddle.route.PathDsl.*
import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.{Chunk, durationInt, Clock, UIO, URIO, ZIO}
import zio.json.*
import zio.json.ast.Json

/** `audience` is the resource access tokens are for (RFC 8707); unset, each token names the client it was issued to. */
final case class ProviderConfig(
    issuer: String,
    audience: Option[String] = None,
    accessTokenTtl: zio.Duration = 15.minutes,
    refreshTokenTtl: zio.Duration = 8.hours,
    idTokenTtl: zio.Duration = 15.minutes,
    authorizationCodeTtl: zio.Duration = 2.minutes,
    sessionTtl: zio.Duration = 1.hour,
)

object Provider:
  def routes(config: ProviderConfig, stores: ProviderStores, key: SigningKey): Routes[Rsa, Nothing] =
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
    def strs(xs: String*) = Json.Arr(Chunk.fromIterable(xs.map(Json.Str(_))))
    Json
      .Obj(
        "issuer"                                -> Json.Str(iss),
        "authorization_endpoint"                -> Json.Str(s"$iss/authorize"),
        "token_endpoint"                        -> Json.Str(s"$iss/token"),
        "userinfo_endpoint"                     -> Json.Str(s"$iss/userinfo"),
        "jwks_uri"                              -> Json.Str(s"$iss/jwks.json"),
        "introspection_endpoint"                -> Json.Str(s"$iss/introspect"),
        "revocation_endpoint"                   -> Json.Str(s"$iss/revoke"),
        "response_types_supported"              -> strs("code", "code id_token"),
        "grant_types_supported"                 -> strs("authorization_code", "client_credentials", "refresh_token"),
        "subject_types_supported"               -> strs("public"),
        "id_token_signing_alg_values_supported" -> strs("RS256"),
        "token_endpoint_auth_methods_supported" -> strs("client_secret_basic", "client_secret_post", "none"),
        "code_challenge_methods_supported"      -> strs("S256"),
        "scopes_supported"                      -> strs("openid", "profile", "email", "offline_access"),
      )
      .toJson
  end discovery

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
          case Some(_) if challenge.isDefined && !method.contains("S256") =>
            ZIO.succeed(Response.badRequest("code_challenge_method must be S256"))
          case Some(c) if c.secretHash.isEmpty && challenge.isEmpty =>
            ZIO.succeed(Response.badRequest("a public client must send a PKCE code_challenge"))
          case Some(_) =>
            (Ids.token <*> Clock.instant).flatMap { (code, now) =>
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
                  val params = Form((List("code" -> code) ++ state.map("state" -> _).toList)*)
                  Response.redirect(redirect + (if redirect.contains("?") then "&" else "?") + params.render)
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
    withForm(req) { form =>
      val user   = form.get("username").getOrElse("")
      val pass   = form.get("password").getOrElse("")
      val resume = form.get("resume").filter(isLocalPath).getOrElse("/")
      stores.users.authenticate(user, pass).flatMap {
        case None    => ZIO.succeed(Response.text("invalid credentials", Status.Unauthorized))
        case Some(u) =>
          (Ids.token <*> Clock.instant).flatMap { (sid, now) =>
            stores.sessions
              .put(SessionRec(sid, u.id, now.plusMillis(config.sessionTtl.toMillis)))
              .as(
                Response
                  .redirect(resume)
                  .addCookie(
                    SetCookie(
                      "op_session",
                      sid,
                      flags = Set(heddle.http.header.CookieFlag.HttpOnly),
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
  ): URIO[Rsa, Response] =
    withForm(req) { form =>
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
  ): URIO[Rsa, Response] =
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
          else issue(config, stores, key, client, Grant(ac.userId, ac.scopes, ac.nonce), now)
        }
    }
  end authCodeGrant

  private def clientCredGrant(
      config: ProviderConfig,
      key: SigningKey,
      client: ClientRecord,
      form: Form,
  ): URIO[Rsa, Response] =
    if client.secretHash.isEmpty then ZIO.succeed(Response.unauthorized("confidential client required"))
    else
      val scope  = form.get("scope").map(_.split(" ").filter(_.nonEmpty).toSet).getOrElse(Set.empty)
      val claims =
        TokenClaims(
          client.id,
          config.issuer.stripSuffix("/"),
          config.audience.getOrElse(client.id),
          scope,
          config.accessTokenTtl,
        )
      signed(Jose.sign(key, claims).map(jsonToken(_, None, None, config.accessTokenTtl.toSeconds)))

  private def refreshGrant(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      client: ClientRecord,
      form: Form,
  ): URIO[Rsa, Response] =
    val tok = form.get("refresh_token").getOrElse("")
    stores.tokens.takeRefresh(tok).flatMap {
      case None                               => ZIO.succeed(Response.badRequest("invalid_grant"))
      case Some(r) if r.clientId != client.id => ZIO.succeed(Response.badRequest("invalid_grant"))
      case Some(r)                            =>
        Clock.instant.flatMap { now =>
          if now.isAfter(r.exp) then ZIO.succeed(Response.badRequest("invalid_grant"))
          else issue(config, stores, key, client, Grant(r.userId, r.scopes, None), now)
        }
    }
  end refreshGrant

  private def issue(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      client: ClientRecord,
      grant: Grant,
      now: Instant,
  ): URIO[Rsa, Response] =
    val iss    = config.issuer.stripSuffix("/")
    val access =
      TokenClaims(grant.userId, iss, config.audience.getOrElse(client.id), grant.scopes, config.accessTokenTtl)
    val idTok = Option.when(grant.scopes.contains("openid"))(
      TokenClaims(grant.userId, iss, client.id, grant.scopes, config.idTokenTtl, grant.nonce.map("nonce" -> _).toMap)
    )
    val refresh =
      if grant.scopes.contains("offline_access") || grant.scopes.contains("openid") then Ids.token.asSome
      else ZIO.none
    signed(
      for
        a  <- Jose.sign(key, access)
        id <- ZIO.foreach(idTok)(Jose.sign(key, _))
        rt <- refresh
        _  <- ZIO.foreachDiscard(rt) { t =>
          stores.tokens.putRefresh(
            RefreshRec(t, client.id, grant.userId, grant.scopes, t, now.plusMillis(config.refreshTokenTtl.toMillis))
          )
        }
      yield jsonToken(a, rt, id, config.accessTokenTtl.toSeconds)
    )
  end issue

  /** Whom a token is for and what it may do. */
  private final case class Grant(userId: String, scopes: Set[String], nonce: Option[String])

  /** A signing failure is the provider's own fault: `500 server_error`. */
  private def signed(response: ZIO[Rsa, RsaError, Response]): URIO[Rsa, Response] =
    response.catchAll(e => ZIO.logError(e.message).as(Response.internalServerError("server_error")))

  private def userinfo(
      config: ProviderConfig,
      stores: ProviderStores,
      key: SigningKey,
      req: Request,
  ): URIO[Rsa, Response] =
    bearer(req) match
      case None      => ZIO.succeed(Response.unauthorized())
      case Some(tok) =>
        Jose
          .verify(tok, key.jwks, Expected(config.issuer.stripSuffix("/"), None))
          .foldZIO(
            _ => ZIO.succeed(Response.unauthorized()),
            c =>
              stores.users.byId(c.subject).map {
                case None    => Response.notFound()
                case Some(u) =>
                  val email = u.claims.getOrElse("email", s"${u.username}@example.test")
                  Response.json(
                    Json
                      .Obj(
                        "sub"                -> Json.Str(u.id),
                        "preferred_username" -> Json.Str(u.username),
                        "email"              -> Json.Str(email),
                      )
                      .toJson
                  )
              },
          )

  private def introspect(config: ProviderConfig, key: SigningKey, req: Request): URIO[Rsa, Response] =
    withForm(req) { form =>
      val tok = form.get("token").getOrElse("")
      Jose
        .verify(tok, key.jwks, Expected(config.issuer.stripSuffix("/"), None))
        .fold(
          _ => Response.json(Json.Obj("active" -> Json.Bool(false)).toJson),
          c =>
            Response.json(
              Json
                .Obj(
                  "active" -> Json.Bool(true),
                  "sub"    -> Json.Str(c.subject),
                  "scope"  -> Json.Str(c.scopes.mkString(" ")),
                )
                .toJson
            ),
        )
    }

  private def revoke(stores: ProviderStores, req: Request): UIO[Response] =
    withForm(req) { form =>
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
        stores.clients.byId(cid).flatMap {
          case None    => ZIO.none
          case Some(c) =>
            c.secretHash match
              case None    => ZIO.some(c)
              case Some(h) => ZIO.blocking(ZIO.succeed(secret.exists(Passwords.check(_, h)))).map(Option.when(_)(c))
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
    val digest = DigestPlatform.sha256Sync(Chunk.fromArray(verifier.getBytes(StandardCharsets.US_ASCII)))
    Base64Url.encode(digest) == challenge

  /** An unreadable form is the client's error, `400 invalid_request` (RFC 6749 §5.2), not a defect. */
  private def withForm[R](req: Request)(use: Form => URIO[R, Response]): URIO[R, Response] =
    req.body.asForm.foldZIO(_ => ZIO.succeed(Response.badRequest("invalid_request")), use)

  private def bearer(req: Request): Option[String] =
    req.headers.get[Authorization] match
      case Some(Authorization(AuthScheme.Bearer, t)) if t.nonEmpty => Some(t)
      case _                                                       => None

  private def jsonToken(access: String, refresh: Option[String], idToken: Option[String], expires: Long): Response =
    val fields =
      Chunk(
        "access_token" -> Json.Str(access),
        "token_type"   -> Json.Str("Bearer"),
        "expires_in"   -> Json.Num(expires),
      ) ++ Chunk.fromIterable(refresh.map("refresh_token" -> Json.Str(_))) ++
        Chunk.fromIterable(idToken.map("id_token" -> Json.Str(_)))
    Response.json(Json.Obj(fields).toJson)

  /** A path on this server: `/login?resume=//evil.example` or `https://...` would make the login an open redirect. */
  private def isLocalPath(s: String): Boolean =
    s.startsWith("/") && !s.startsWith("//") && !s.contains('\\')

  private def escape(s: String): String =
    s.replace("&", "&amp;").replace("\"", "&quot;").replace("'", "&#39;").replace("<", "&lt;").replace(">", "&gt;")
end Provider
