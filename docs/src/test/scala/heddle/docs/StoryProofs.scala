package heddle.docs

import heddle.*
import heddle.docs.fixture.*
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
    test("User schema is named"):
      assertTrue(summon[Schema[User]].named.contains("User"))
    ,
    test("getUser doc is GET /users/{id} promoted get_users_id"):
      val d = Users.getUser.doc
      assertTrue(
        d.method == Method.GET,
        d.pathTemplate == "/users/{id}",
        d.promoted,
        d.toolName == "get_users_id",
        d.hints.contains(Hint.ReadOnly),
      )
    ,
    test("HTTP GET /users/1 is Ada"):
      Users.seed.flatMap { (store, nextId) =>
        Users.api(store, nextId).routes(Request.get("/users/1")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString.contains("Ada"))
        }
      }
    ,
    test("HTTP GET /users lists Ada"):
      Users.seed.flatMap { (store, nextId) =>
        Users.api(store, nextId).routes(Request.get("/users")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString.contains("Ada"))
        }
      }
    ,
    test("HTTP GET /users/99 is 404"):
      Users.seed.flatMap { (store, nextId) =>
        val users = Users.api(store, nextId)
        for
          ok   <- users.routes(Request.get("/users/1"))
          miss <- users.routes(Request.get("/users/99"))
        yield assertTrue(ok.status == Status.Ok, ok.body.asString.contains("Ada"), miss.status == Status.NotFound)
      }
    ,
    test("MCP tools/call get_users_id is Ada"):
      Users.seed.flatMap { (store, nextId) =>
        ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
          val call = Users.rpc(
            "tools/call",
            obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
          )
          mcp.handle(call).map { out =>
            val json = out.get.toJson
            assertTrue(json.contains("Ada"), !json.contains("\"isError\":true"))
          }
        }
      }
    ,
    test("MCP ping completes"):
      Users.seed.flatMap { (store, nextId) =>
        ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
          mcp.handle(Users.rpc("ping", obj())).map { out =>
            assertTrue(out.get.toJson.contains("\"resultType\":\"complete\""))
          }
        }
      }
    ,
    test("tools/list is promoted-only"):
      Users.seed.flatMap { (store, nextId) =>
        ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
          mcp.handle(Users.rpc("tools/list", obj())).map { out =>
            val json = out.get.toJson
            assertTrue(json.contains("get_users_id"), json.contains("get_users"), !json.contains("post_users"))
          }
        }
      }
    ,
    test("OpenAPI JSON is a projection of the Api"):
      Users.seed.map { (store, nextId) =>
        val json = Users.api(store, nextId).openApi.toJson
        assertTrue(json.contains("/users/{id}"), json.contains("\"get\""), json.contains("Get a user"))
      }
    ,
    test("landing HTML is the manifesto"):
      ascent.html.Html.renderPage(Landing.document).map { page =>
        assertTrue(
          page.html.contains("Write the service once."),
          !page.html.contains("libraryDependencies"),
          page.html.contains("data-specular-mount=\"landing-poster\""),
          page.html.contains("data-specular-mount=\"desk-app\""),
        )
      },
  )
end StoryProofs
