package heddle

import zio.*
import zio.test.*

final case class User(name: String)
trait Ping:
  def ping: String

object AuthSpec extends ZIOSpecDefault:
  def spec =
    suite("Auth")(
      test("basicAuth accepts matching credentials"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.basicAuth("ada", "pw")
        val req    = Request.get("/x").copy(headers = Headers.empty.set(Authorization.basic("ada", "pw")))
        routes(req).map(res => assertTrue(res.status == Status.Ok, res.body.asString == "ok"))
      ,
      test("basicAuth rejects missing credentials with 401"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.basicAuth("ada", "pw")
        routes(Request.get("/x")).map { res =>
          assertTrue(
            res.status == Status.Unauthorized,
            res.header("WWW-Authenticate").exists(_.startsWith("Basic")),
          )
        }
      ,
      test("bearerAuth accepts a valid token"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.bearerAuth(_ == "secret")
        val req    = Request.get("/x").addHeader("Authorization", "Bearer secret")
        routes(req).map(res => assertTrue(res.body.asString == "ok"))
      ,
      test("provided injects User while keeping another service"):
        val ping = ZLayer.succeed(new Ping:
          def ping = "pong")
        val authed =
          Routes(
            Method.GET / "me" -> handler(
              for
                u <- ZIO.service[User]
                p <- ZIO.service[Ping]
              yield Response.text(u.name + p.ping)
            )
          )
        val routes: Routes[Ping, Nothing] = authed.provided[User, Ping](_ => ZIO.succeed(User("ada")))
        routes(Request.get("/me")).provide(ping).map(res => assertTrue(res.body.asString == "adapong"))
      ,
      test("Auth.bearer lookup feeds provided"):
        val authed =
          Routes(Method.GET / "me" -> handler(ZIO.serviceWith[User](u => Response.text(u.name))))
        val routes = authed.provided(
          Auth.bearer(t => if t == "t1" then ZIO.succeed(User("ada")) else ZIO.fail(Auth.unauthorizedBearer))
        )
        val req = Request.get("/me").addHeader("Authorization", "Bearer t1")
        routes(req).map(res => assertTrue(res.body.asString == "ada"))
      ,
      test("OpenAPI emits securitySchemes and operation security"):
        val ep   = Endpoint.get("me").outText().auth(SecurityScheme.HttpBearer())
        val json = OpenApi.from("Api", "1.0.0", ep).toJson
        assertTrue(
          json.contains("\"securitySchemes\""),
          json.contains("\"bearer\""),
          json.contains("\"security\""),
          json.contains("\"401\""),
        )
      ,
      test("Swagger UI persists authorization"):
        val html = SwaggerUI.page("/docs/openapi.json")
        assertTrue(html.contains("persistAuthorization: true")),
    ) @@ TestAspect.timeout(5.seconds)
end AuthSpec
