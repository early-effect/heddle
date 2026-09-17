package heddle.docs

import heddle.docs.fixture.*
import heddle.docs.ui.Hub
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
MCP is a host protocol over `BoundOp`. The tutorial is [Agents fall out](agents-fall-out.html).
This page is the remaining knobs.

Mark ops with `.mcp`. `Mcp.from(api)` fails on duplicate tool names or non-promotable shapes.
Protocol revision is **2026-07-28** only. Native tools (`.tool("name")(f)`) sit beside bound ops
when you need something that is not an HTTP operation.
""",
    section("Promote vs catalog")(
      md"""
`Hint.ReadOnly` / `Destructive` / `Idempotent` / `OpenWorld` travel with the tool.
`withCatalog` adds `search_operations` and `invoke`.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store)).map(_.withCatalog).flatMap { mcp =>
            mcp.handle(BoxOffice.rpc("tools/list", obj())).map { out =>
              val json = out.get.toJson
              json.contains("search_operations") && json.contains("invoke")
            }
          }
        }
      }.assert(catalog => assertTrue(catalog)),
      md"""
The live catalog toggle is on [Agents fall out](agents-fall-out.html).
""",
    ),
    section("JSON-RPC")(
      md"""
`tools/call` with `get_show` is `GET /shows/{id}`.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
            val call = BoxOffice.rpc(
              "tools/call",
              obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
            )
            mcp.handle(call).map(_.get.toJson.contains("Evening bill"))
          }
        }
      }.assert(ok => assertTrue(ok)),
      illustrationIO(Hub.Lives.rpc).live.withMountKey(InteractiveRegistry.JsonRpcInspector),
    ),
    section("HTTP and stdio")(
      md"""
`mcp.routes` is Streamable HTTP at `/mcp` (`mcp.at("other")` to move it). `mcp.stdio()` reads
JSON-RPC lines from stdin and writes lines to stdout.

Do not embed a second authorization server inside `heddle-mcp`. When the HTTP transport needs a
bearer token, `Mcp.bearer` + `Mcp.protectedResource` advertise the resource metadata. JWT
verification lives in `heddle-oauth`.
"""
    ),
  )
end Agents
