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

object Agents extends DocSpecSuite:

  def doc = page("Agents")(
    md"""
MCP is a host protocol over `BoundOp`, not a second tool DSL. Mark the ops agents should see
with `.mcp` (or `.mcp("explicit_name")`). `Mcp.from(api)` fails on duplicate tool names or
non-promotable shapes. Protocol revision is **2026-07-28** only.
""",
    section("Promote, don't auto-export")(
      md"""
`.mcp` sets `promoted`. `Hint.ReadOnly` / `Destructive` / `Idempotent` / `OpenWorld` travel with
the tool. JSON in and JSON out are required (`OpArgs.promotable`). Form, bytes, and SSE stay on
HTTP; do not flatten them into tools.

`tools/list` is promoted-only. `withCatalog` adds `search_operations` and `invoke` so agents can
still find the rest of the `Api`.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
            mcp.handle(Users.rpc("tools/list", obj())).map { out =>
              val json = out.get.toJson
              (
                json.contains("get_users_id"),
                json.contains("get_users"),
                !json.contains("post_users"),
              )
            }
          }
        }
      }.assert { case (getId, list, noCreate) =>
        assertTrue(getId, list, noCreate)
      },
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).map(_.withCatalog).flatMap { mcp =>
            mcp.handle(Users.rpc("tools/list", obj())).map { out =>
              val json = out.get.toJson
              json.contains("search_operations") && json.contains("invoke")
            }
          }
        }
      }.assert(catalog => assertTrue(catalog)),
      exampleDom(InteractiveRegistry.McpCatalog)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/McpCatalog.scala", "demo"),
    ),
    section("Call the same function")(
      md"""
`tools/call` with `get_users_id` is `GET /users/{id}`. Native tools (`.tool("name")(f)`) sit
beside bound ops when you need something that is not an HTTP operation.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          ZIO.fromEither(Users.mcpOf(store, nextId)).flatMap { mcp =>
            val call = Users.rpc(
              "tools/call",
              obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
            )
            mcp.handle(call).map(_.get.toJson.contains("Ada"))
          }
        }
      }.assert(ok => assertTrue(ok)),
      exampleDom(InteractiveRegistry.JsonRpcInspector)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/JsonRpcInspector.scala", "demo"),
    ),
    section("HTTP and stdio")(
      md"""
`mcp.routes` is Streamable HTTP at `/mcp` (`mcp.at("other")` to move it). `mcp.stdio()` reads
JSON-RPC lines from stdin and writes lines to stdout. The example process is
`sbt "example/run -- --mcp-stdio"`.

Do not embed a second authorization server inside `heddle-mcp`. When the HTTP transport needs a
bearer token, `Mcp.bearer` + `Mcp.protectedResource` advertise the resource metadata. JWT
verification lives in `heddle-oauth`.
"""
    ),
  )
end Agents
