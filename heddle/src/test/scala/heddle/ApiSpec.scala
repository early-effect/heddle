package heddle

import zio.*
import zio.json.JsonCodec
import zio.test.*

final case class Item(id: Int, name: String) derives Schema, JsonCodec
final case class NewItem(name: String) derives Schema, JsonCodec

object ApiSpec extends ZIOSpecDefault:

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
        bound.input(Request.get("/items/3")).map(id => assertTrue(id == 3))
      ,
      test("job promotes; resource does not"):
        val getItem    = Endpoint.get("items" / int("id")).out[Item].name("get_item")
        val createItem = Endpoint.post("items").inJson[NewItem].out[Item].name("create_item")
        val api        = Api("Shop", "1.0.0")
          .resource(createItem)(n => ZIO.succeed(Item(2, n.name)))
          .job(getItem)(id => ZIO.succeed(Item(id, "x")))
        val byName = api.openApi.endpoints.map(d => d.toolName -> d.promoted).toMap
        assertTrue(byName.get("get_item").contains(true), byName.get("create_item").contains(false))
      ,
      test("resource unpromotes an endpoint that was marked .mcp"):
        val ep  = Endpoint.get("items" / int("id")).out[Item].mcp("get_item")
        val api = Api("Shop", "1.0.0").resource(ep)(id => ZIO.succeed(Item(id, "x")))
        assertTrue(api.openApi.endpoints.head.promoted == false)
      ,
      test("job of an already-promoted endpoint keeps the mcp name"):
        val ep  = Endpoint.get("items" / int("id")).out[Item].mcp("get_item")
        val api = Api("Shop", "1.0.0").job(ep)(id => ZIO.succeed(Item(id, "x")))
        val d   = api.openApi.endpoints.head
        assertTrue(d.promoted, d.toolName == "get_item"),
    ) @@ TestAspect.timeout(5.seconds)
end ApiSpec
