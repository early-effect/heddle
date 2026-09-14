package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.mcp.protocol.JsonRpc.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

object ThreeHosts extends DocSpecSuite:

  def doc = page("One capability, three hosts")(
    md"""
This page is the whole story in one file. The users API is a handful of `Endpoint`s bound once.
HTTP, MCP Streamable HTTP, and stdio are hosts of that bind. They are not three implementations.
""",
    section("Write the capability once")(
      md"""
`getUser` is an `Endpoint`. `.mcp` promotes it as a tool. `Api.bind` is the function agents and
HTTP both run.
""",
      exampleValue {
        Users.getUser.doc.toolName
      }.assert(name => assertTrue(name == "get_users_id")),
    ),
    section("HTTP")(
      md"""
`api.routes` is `Routes`. A GET is an ordinary `Request`. Humans hit this from a browser or
Swagger. Systems hit it from `Client`.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          Users.api(store, nextId).routes(Request.get("/users/1")).map { res =>
            (res.status, res.body.asString)
          }
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body.contains("Ada"), body.contains("\"id\":1") || body.contains("\"id\": 1"))
      },
    ),
    section("MCP Streamable HTTP")(
      md"""
`Mcp.from(api)` speaks JSON-RPC on `POST /mcp`. `tools/list` is the promoted set.
`tools/call` with `get_users_id` runs the same function as `GET /users/1`.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
            val call = Users.rpc(
              "tools/call",
              obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
            )
            mcp.handle(call).map { out =>
              val json = out.get.toJson
              json.contains("Ada") && !json.contains("\"isError\":true")
            }
          }
        }
      }.assert(ok => assertTrue(ok)),
    ),
    section("stdio")(
      md"""
`mcp.stdio()` is the same engine on a pipe: one JSON-RPC line in, one line out. No child process
in this example. `handle` is what stdio calls per line. Local agent runtimes spawn
`sbt "example/run -- --mcp-stdio"` (or a published main) and speak that framing.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
            mcp.handle(Users.rpc("ping", obj())).map { out =>
              out.get.toJson.contains("\"resultType\":\"complete\"")
            }
          }
        }
      }.assert(ok => assertTrue(ok)),
    ),
    section("Toggle the host")(
      md"""
The widget below is the same get-user operation as HTTP, as `tools/call`, and as a stdio line.
The payloads match what the examples above just asserted.
""",
      exampleDom(InteractiveRegistry.ThreeHosts)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/ThreeHostsExplorer.scala", "demo"),
    ),
  )
end ThreeHosts
