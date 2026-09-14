package example

import java.nio.file.{Files as JFiles, Path}
import heddle.*
import heddle.json.given
import heddle.mcp.Mcp
import heddle.oauth.jose.SigningKey
import heddle.oauth.provider.*
import heddle.oauth.rs.{JwtClaim, JwtVerifier}
import heddle.sse.*
import zio.*
import zio.json.JsonCodec

final case class User(id: Int, name: String) derives Schema, JsonCodec
final case class NewUser(name: String) derives Schema, JsonCodec
final case class NotFound(message: String) derives Schema, JsonCodec
final case class Me(sub: String, scopes: List[String]) derives Schema, JsonCodec

object Main extends ZIOAppDefault:
  private val issuer = "http://localhost:8080"

  private val getUser =
    Endpoint
      .get("users" / int("id"))
      .out[User]
      .outError[NotFound](Status.NotFound)
      .summary("Get a user")
      .tag("users")

  private val createUser =
    Endpoint
      .post("users")
      .inJson[NewUser]
      .out[User](Status.Created)
      .auth(
        SecurityScheme.OAuth2(
          "oauth2",
          OAuthFlows(
            authorizationCode = Some(
              OAuthFlow(
                authorizationUrl = Some(s"$issuer/authorize"),
                tokenUrl = Some(s"$issuer/token"),
                scopes = Map("openid" -> "OpenID", "profile" -> "Profile"),
              )
            )
          ),
        )
      )
      .summary("Create a user")
      .tag("users")

  private val listUsers =
    Endpoint
      .get("users")
      .out[List[User]]
      .summary("List users")
      .tag("users")

  private val me =
    Endpoint
      .get("me")
      .out[Me]
      .auth(SecurityScheme.HttpBearer(bearerFormat = Some("JWT")))
      .summary("Current subject")
      .tag("auth")

  def run =
    ZIO.serviceWithZIO[ZIOAppArgs] { args =>
      if args.getArgs.contains("--mcp-stdio") then runStdio else runHttp
    }

  private def runStdio =
    (for
      store <- Ref.make(Map(1 -> User(1, "Ada")))
      mcp   <- ZIO.fromEither(Mcp.from(publicApi(store))).mapError(IllegalArgumentException(_))
      _     <- mcp.stdio()
    yield ()).provideLayer(Runtime.removeDefaultLoggers)

  private def runHttp =
    val key  = SigningKey.generateRsa("op")
    val meta = s"$issuer/.well-known/oauth-protected-resource"
    for
      store  <- Ref.make(Map(1 -> User(1, "Ada")))
      nextId <- Ref.make(2)
      stores <- MemoryStores.seed(
        List(UserRecord("u1", "ada", Passwords.hash("ada"), Map("email" -> "ada@example.test"))),
        List(
          ClientRecord("web", None, List(s"$issuer/docs"), Set("authorization_code", "refresh_token")),
          ClientRecord("swagger", None, List(s"$issuer/docs/oauth2-redirect.html"), Set("authorization_code")),
          ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
        ),
      )
      verifier <- JwtVerifier.static(key.publicJwksJson, issuer, "")
      users  = publicApi(store)
      writes = authedApi(store, nextId)
      mcp <- ZIO.fromEither(Mcp.from(users, writes)).mapError(IllegalArgumentException(_)).map(_.withCatalog)
      op        = Provider.routes(ProviderConfig(issuer), stores, key)
      authed    = writes.routes.provided(Auth.bearer(t => verifier.verify(t).mapError(_.toResponse)))
      mcpAuthed = mcp.routes.provided(
        Mcp.bearer(meta, List("openid", "profile"))(t =>
          verifier.verify(t).mapError(_ => Mcp.unauthorized(meta, List("openid", "profile")))
        )
      )
      admin  = Routes(Method.GET / "admin" -> Handler.text("admin-ok")) @@ Middleware.basicAuth("admin", "admin")
      docs   = Api.openApi("Heddle example", "0.1.0", users, writes).routes("docs")
      prm    = Mcp.protectedResource(s"$issuer/mcp", List(issuer), List("openid", "profile"))
      routes = users.routes ++ authed ++ mcpAuthed ++ prm ++ admin ++ op ++ preview ++ docs
      _ <- ZIO.logInfo("listening on http://localhost:8080/docs and /mcp (Authorize against the embedded OP)")
      _ <- Server.sbtInterruptExit
      _ <- Server
        .serve(routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug))
        .provide(Server.Config.defaults)
        .catchAllCause(c => if c.isInterruptedOnly then ZIO.unit else ZIO.refailCause(c))
    yield ()
    end for
  end runHttp

  private[example] def publicApi(store: Ref[Map[Int, User]]): Api[Any] =
    Api("Heddle example", "0.1.0")
      .bind(getUser.mcp.hints(Hint.ReadOnly)) { id =>
        store.get.map(_.get(id).toRight(NotFound(s"user $id"))).flatMap(ZIO.fromEither)
      }
      .bind(listUsers.mcp.hints(Hint.ReadOnly)) { _ =>
        store.get.map(_.values.toList.sortBy(_.id))
      }

  private def authedApi(store: Ref[Map[Int, User]], nextId: Ref[Int]): Api[JwtClaim] =
    Api("Heddle example", "0.1.0")
      .bind(createUser.mcp) { body =>
        ZIO.service[JwtClaim] *>
          nextId.modify(n => n -> (n + 1)).flatMap { id =>
            val user = User(id, body.name)
            store.update(_ + (id -> user)).as(user)
          }
      }
      .bind(me) { _ =>
        ZIO.serviceWith[JwtClaim](c => Me(c.subject, c.scopes.toList.sorted))
      }

  private def preview: Routes[Any, Response] =
    val dir = previewDir
    Routes(
      Method.GET / "preview"                 -> handler(file(dir.resolve("index.html"))),
      Method.GET / "preview" / "preview.css" -> handler(file(dir.resolve("preview.css"))),
      Method.GET / "__preview" / "reload"    -> handler {
        Sse.session { w =>
          w.send(ServerSentEvent("ok", event = Some("reload"))) *> ZIO.never
        }
      },
      Method.GET / "session" -> handler {
        Sse.session { w =>
          w.send(ServerSentEvent("hello"))
        }
      },
    )
  end preview

  private def file(path: Path): UIO[Response] =
    Files.fromPath(path).fold(_ => Response.notFound(), identity)

  /** `sbt example/run` cwd is the example module; a repo-root run still works. */
  private def previewDir: Path =
    val here     = Path.of("preview")
    val fromRoot = Path.of("example", "preview")
    if JFiles.isDirectory(here) then here else fromRoot
end Main
