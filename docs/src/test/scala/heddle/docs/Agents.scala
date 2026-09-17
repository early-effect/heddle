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
Native protocol is **2026-07-28**. Native tools (`.tool("name")(f)`) sit beside bound ops
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
    section("Protocol eras")(
      md"""
Native MCP is **2026-07-28**: `server/discover`, version in `_meta`, POST only, no session store.
Grok Build, Cursor, and Claude Code still send `initialize`. HTTP and stdio answer that handshake
so those hosts can list tools and call one. Authors do not opt in.

| | Native (2026-07-28) | Compat (2025-11-25) |
| --- | --- | --- |
| Handshake | `server/discover` | `initialize` / `notifications/initialized` |
| Session | none | HTTP mints an ephemeral `Mcp-Session-Id` and echoes it. It is not looked up. stdio has no session header. |
| HTTP | POST | POST; GET 405; DELETE 200 |
| stdio | `_meta` on every line | `initialize` then the rest of that process is 2025 |

`initialize` selects compat. `MCP-Protocol-Version: 2026-07-28`, `_meta` version `2026-07-28`, or
`server/discover` selects native. Missing 2026 headers on a non-initialize POST is still an error.

GET is 405 on purpose. A live SSE back-channel is how 2025 hosts asked the client for sampling
and elicitation. 2026 replaced that with MRTR. Do not grow a GET stream as "the" notification path.
"""
    ),
    section("HTTP and stdio")(
      md"""
`mcp.routes` is Streamable HTTP at `/mcp` (`mcp.at("other")` to move it). `mcp.stdio()` reads
JSON-RPC lines from stdin and writes lines to stdout. Both transports speak native 2026 and the
2025 handshake.

Do not embed a second authorization server inside `heddle-mcp`. When the HTTP transport needs a
bearer token, `Mcp.bearer` + `Mcp.protectedResource` advertise the resource metadata. JWT
verification lives in `heddle-oauth`.
"""
    ),
  )
end Agents
