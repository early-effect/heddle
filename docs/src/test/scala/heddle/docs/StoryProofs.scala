package heddle.docs

import heddle.*
import heddle.docs.fixture.BoxOffice
import heddle.docs.fixture.Show
import heddle.mcp.protocol.JsonRpc.*
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

/** JVM proofs that used to sit on Start / Tutorial pages as `exampleZIO` chrome. */
object StoryProofs extends ZIOSpecDefault:

  def spec = suite("Story proofs")(
    test("GET /health is 200 ok"):
      val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
      routes(Request.get("/health")).map(res => assertTrue(res.status == Status.Ok, res.body.asString == "ok"))
    ,
    test("a one-file server answers GET /users/7"):
      val routes = Routes(
        Method.GET / "health"            -> Handler.text("ok"),
        Method.GET / "users" / int("id") -> { (id: Int) =>
          ZIO.succeed(Response.text(id.toString))
        },
      )
      val app = routes @@ (Middleware.requestId() ++ Middleware.cors())
      app(Request.get("/users/7")).map(res => assertTrue(res.status == Status.Ok, res.body.asString == "7"))
    ,
    test("Show schema is named"):
      assertTrue(summon[Schema[Show]].named.contains("Show"))
    ,
    test("getShow doc is GET /shows/{id} promoted get_show"):
      val d = BoxOffice.getShow.doc
      assertTrue(
        d.method == Method.GET,
        d.pathTemplate == "/shows/{id}",
        d.toolName == "get_show",
        d.hints.contains(Hint.ReadOnly),
      )
    ,
    test("HTTP GET /shows/1 is Evening bill"):
      BoxOffice.seed.flatMap { store =>
        BoxOffice.api(store).routes(Request.get("/shows/1")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString.contains("Evening bill"))
        }
      }
    ,
    test("HTTP GET /shows lists Evening bill"):
      BoxOffice.seed.flatMap { store =>
        BoxOffice.api(store).routes(Request.get("/shows")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString.contains("Evening bill"))
        }
      }
    ,
    test("HTTP GET /shows/99 is 404"):
      BoxOffice.seed.flatMap { store =>
        val api = BoxOffice.api(store)
        for
          ok   <- api.routes(Request.get("/shows/1"))
          miss <- api.routes(Request.get("/shows/99"))
        yield assertTrue(
          ok.status == Status.Ok,
          ok.body.asString.contains("Evening bill"),
          miss.status == Status.NotFound,
        )
      }
    ,
    test("MCP tools/call get_show is Evening bill"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          val call = BoxOffice.rpc(
            "tools/call",
            obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
          )
          mcp.handle(call).map { out =>
            val json = out.get.toJson
            assertTrue(json.contains("Evening bill"), !json.contains("\"isError\":true"))
          }
        }
      }
    ,
    test("MCP ping completes"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          mcp.handle(BoxOffice.rpc("ping", obj())).map { out =>
            assertTrue(out.get.toJson.contains("\"resultType\":\"complete\""))
          }
        }
      }
    ,
    test("tools/list is the job set"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          mcp.handle(BoxOffice.rpc("tools/list", obj())).map { out =>
            val json = out.get.toJson
            assertTrue(
              json.contains("get_show"),
              json.contains("list_shows"),
              json.contains("seat_the_party"),
              !json.contains("create_hold"),
            )
          }
        }
      }
    ,
    test("OpenAPI JSON is a projection of the Api"):
      BoxOffice.seed.map { store =>
        val json = BoxOffice.api(store).openApi.toJson
        assertTrue(json.contains("/shows/{id}"), json.contains("\"get\""), json.contains("One bill"))
      }
    ,
    test("landing HTML is the manifesto"):
      ascent.html.Html.renderPage(Landing.document).map { page =>
        assertTrue(
          page.html.contains("One bind. Three runtimes."),
          page.html.contains("JVM"),
          page.html.contains("Node"),
          page.html.contains("Native"),
          !page.html.contains("libraryDependencies"),
          page.html.contains("data-specular-mount=\"landing-poster\""),
          page.html.contains("data-specular-mount=\"desk-app\""),
        )
      },
  )
end StoryProofs
