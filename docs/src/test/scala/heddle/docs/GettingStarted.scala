package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.mcp.protocol.JsonRpc.*
import specular.*
import zio.json.EncoderOps
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object GettingStarted extends DocSpecSuite:

  def doc = page("Getting started")(
    md"""
Add `heddle` (the header install panel has the current version). JSON, MCP, OAuth, and brotli are
optional artifacts on the same version. JDK 21+.

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "<version>"
libraryDependencies += "rocks.earlyeffect" %% "heddle-zio-json" % "<version>" // when you want JSON
libraryDependencies += "rocks.earlyeffect" %% "heddle-mcp" % "<version>"      // when agents show up
```
""",
    section("A server in one file")(
      md"""
`import heddle.*` is the facade. SSE, WebSocket, and Datastar stay in their own packages.

You can stop here: this is a real HTTP service.
""",
      exampleZIO {
        val routes = Routes(
          Method.GET / "health"            -> Handler.text("ok"),
          Method.GET / "users" / int("id") -> { (id: Int) =>
            ZIO.succeed(Response.text(id.toString))
          },
        )
        val app = routes @@ (Middleware.requestId() ++ Middleware.cors())
        app(Request.get("/users/7")).map(res => (res.status, res.body.asString))
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body == "7")
      },
      md"""
To bind a port:

```scala
object Hello extends HeddleApp:
  def routes = app
```

`HeddleApp` serves `routes` and exits 0 on Ctrl-C under sbt 2. `Server.serve(app).provide(Server.Config.defaults)`
is the same thing without the trait.
""",
    ),
    section("The same bind becomes an API")(
      md"""
`Endpoint` is documentation and decoding. `Api.bind` is the implementation. OpenAPI is a projection
of that, not a second source of truth. `.inJson` / `.out` need a `JsonCodec` in scope:
`import heddle.json.given` after adding `heddle-zio-json`.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          val users = Users.api(store, nextId)
          val spec  = users.openApi
          users.routes(Request.get("/users/1")).map { res =>
            (res.status, res.body.asString, spec.toJson.contains("/users/{id}"))
          }
        }
      }.assert { case (status, body, specHasPath) =>
        assertTrue(status == Status.Ok, body.contains("Ada"), specHasPath)
      },
      md"""
`spec.routes("docs")` serves Swagger UI at `/docs` and the spec at `/docs/openapi.json`.
""",
    ),
    section("Promote the same ops to agents")(
      md"""
`.mcp` marks an operation as an MCP tool. `Mcp.from(api)` hosts those tools on Streamable HTTP
(`mcp.routes`, default `POST /mcp`) or stdio (`mcp.stdio()`). Protocol revision is 2026-07-28 only.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
            mcp.handle(Users.rpc("tools/list", obj())).map { out =>
              val json = out.get.toJson
              json.contains("get_users_id") && json.contains("get_users")
            }
          }
        }
      }.assert(listed => assertTrue(listed)),
      md"""
The runnable example in this repo is both hosts in one process:

```bash
sbt example/run                      # HTTP + Swagger at /docs, MCP at POST /mcp
sbt "example/run -- --mcp-stdio"     # same Api, stdio JSON-RPC
```
""",
    ),
  )
end GettingStarted
