package heddle

import zio.*
import zio.test.*

final case class Item(id: Int, name: String) derives Schema
final case class NewItem(name: String) derives Schema

object ApiSpec extends ZIOSpecDefault:
  given JsonCodec[Item]    = JsonCodec.from(_ => """{"id":1,"name":"a"}""", _ => Right(Item(1, "a")))
  given JsonCodec[NewItem] = JsonCodec.from(_ => """{"name":"a"}""", s => Right(NewItem(s)))

  def spec =
    suite("Api")(
      test("bind feeds routes and openapi from one list"):
        val getItem = Endpoint.get("items" / int("id")).out[Item].summary("Get item").mcp
        val api     = Api("Shop", "1.0.0").bind(getItem) { id => ZIO.succeed(Item(id, "x")) }
        for res <- api.routes(Request.get("/items/7"))
        yield assertTrue(
          res.status == Status.Ok,
          api.openApi.endpoints.length == 1,
          api.openApi.endpoints.head.promoted,
          api.openApi.title == "Shop",
        )
      ,
      test("implement still is a Routes"):
        val ep     = Endpoint.get("hello").outText()
        val routes = ep.implement(_ => ZIO.succeed("world"))
        routes(Request.get("/hello")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString == "world")
        }
      ,
      test("mcp name and hints land on EndpointDoc"):
        val ep = Endpoint.get("users" / int("id")).out[Item].mcp("get_user").hints(Hint.ReadOnly)
        assertTrue(
          ep.doc.promoted,
          ep.doc.mcpName.contains("get_user"),
          ep.doc.toolName == "get_user",
          ep.doc.hints == List(Hint.ReadOnly),
        )
      ,
      test("derived tool name slugs method and path"):
        val ep = Endpoint.get("users" / int("id")).out[Item]
        assertTrue(ep.doc.toolName == "get_users_id")
      ,
      test("BoundOp.input decodes without encoding the response"):
        val ep    = Endpoint.get("items" / int("id")).out[Item]
        val bound = ep.implement(id => ZIO.succeed(Item(id, "x")))
        bound.input(Request.get("/items/3")).map(id => assertTrue(id == 3)),
    ) @@ TestAspect.timeout(5.seconds)
end ApiSpec
