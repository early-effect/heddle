package example

import java.nio.file.{Files as JFiles, Path}
import heddle.*
import heddle.mcp.Mcp
import heddle.oauth.jose.SigningKey
import heddle.oauth.provider.*
import heddle.oauth.rs.{JwtClaim, JwtVerifier}
import heddle.sse.*
import zio.*

object Main extends ZIOAppDefault:
  private val issuer = "http://localhost:8080"

  def run =
    ZIO.serviceWithZIO[ZIOAppArgs] { args =>
      if args.getArgs.contains("--mcp-stdio") then runStdio else runHttp
    }

  private def runStdio =
    (for
      office <- BoxOffice.seed
      mcp    <- ZIO.fromEither(Mcp.from(publicApi(office), writeApi(office))).mapError(IllegalArgumentException(_))
      _      <- mcp.stdio()
    yield ()).provideLayer(Runtime.removeDefaultLoggers)

  private def runHttp =
    val key  = SigningKey.generateRsa("op")
    val meta = s"$issuer/.well-known/oauth-protected-resource"
    for
      office <- BoxOffice.seed
      stores <- MemoryStores.seed(
        List(UserRecord("u1", "ada", Passwords.hash("ada"), Map("email" -> "ada@example.test"))),
        List(
          ClientRecord("web", None, List(s"$issuer/docs"), Set("authorization_code", "refresh_token")),
          ClientRecord("swagger", None, List(s"$issuer/docs/oauth2-redirect.html"), Set("authorization_code")),
          ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
        ),
      )
      verifier <- JwtVerifier.static(key.publicJwksJson, issuer, "")
      public = publicApi(office)
      writes = writeApi(office)
      meApi  = Api("Box office", "0.1.0").resource(Endpoints.me) { _ =>
        ZIO.serviceWith[JwtClaim](c => Me(c.subject, c.scopes.toList.sorted))
      }
      mcp <- ZIO.fromEither(Mcp.from(public, writes)).mapError(IllegalArgumentException(_)).map(_.withCatalog)
      op        = Provider.routes(ProviderConfig(issuer), stores, key)
      authed    = (writes.routes ++ meApi.routes).provided(Auth.bearer(t => verifier.verify(t).mapError(_.toResponse)))
      mcpAuthed = mcp.routes.provided(
        Mcp.bearer(meta, List("openid", "profile"))(t =>
          verifier.verify(t).mapError(_ => Mcp.unauthorized(meta, List("openid", "profile")))
        )
      )
      admin  = Routes(Method.GET / "admin" -> Handler.text("admin-ok")) @@ Middleware.basicAuth("admin", "admin")
      docs   = Api.openApi("Heddle example", "0.1.0", public, writes, meApi).routes("docs")
      prm    = Mcp.protectedResource(s"$issuer/mcp", List(issuer), List("openid", "profile"))
      routes = preview ++ docs ++ public.routes ++ prm ++ op ++ admin ++ authed ++ mcpAuthed
      _ <- ZIO.logInfo("listening on http://localhost:8080/docs and /mcp (Authorize against the embedded OP)")
      _ <- Server.sbtInterruptExit
      _ <- Server
        .serve(routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug))
        .provide(Server.Config.defaults)
        .catchAllCause(c => if c.isInterruptedOnly then ZIO.unit else ZIO.refailCause(c))
    yield ()
    end for
  end runHttp

  private[example] def publicApi(office: BoxOffice): Api[Any] =
    Api("Box office", "0.1.0")
      .job(Endpoints.listShows)(_ => office.listShows)
      .job(Endpoints.getShow)(office.get)
      .resource(Endpoints.listSeats)(office.remainingSeats)
      .job(Endpoints.pickup)(office.pickup)

  private[example] def writeApi(office: BoxOffice): Api[Any] =
    Api("Box office", "0.1.0")
      .resource(Endpoints.createHold(issuer))(office.hold)
      .resource(Endpoints.createOrder(issuer))(office.orderFromHold)
      .job(Endpoints.seatTheParty(issuer))(office.seat)

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

  private def previewDir: Path =
    val here     = Path.of("preview")
    val fromRoot = Path.of("example", "preview")
    if JFiles.isDirectory(here) then here else fromRoot
end Main
