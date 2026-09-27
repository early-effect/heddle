package heddle.docs

import heddle.Client
import heddle.docs.fixture.*
import heddle.docs.ui.Hub
import heddle.mcp.ServedResource
import heddle.mcp.client.{McpCallFailure, McpClient}
import heddle.mcp.protocol.{CallToolResult, ClientRequest, ExtensionId, Implementation, Resource, ToolName}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object Agents extends DocSpecSuite:

  def doc = page("Agents")(
    md"""
MCP is a host protocol over `BoundOp`. The tutorial is [Agents fall out](agents-fall-out.html).
This page is the remaining knobs.

Mark ops with `.mcp`. `Mcp.from(api)` returns every reason it cannot build (a duplicate tool name,
a name outside the MCP grammar, a non-promotable shape) as `McpBuildError`s, before a client ever
connects. Native protocol is **2026-07-28**. Native tools sit beside bound ops when you need
something that is not an HTTP operation: `mcp.tool[Query]("search_users") { q => ... }`. The
name is checked at compile time and `Query` is pinned, so the function needs no type ascription.

The wire types live in `heddle-mcp-protocol`, which depends only on zio-json. A request is
`Message.stateless(id, ClientRequest.CallTool(ToolName("get_show"), args))`.
""",
    section("Promote vs catalog")(
      md"""
`Hint.ReadOnly` / `Destructive` / `Idempotent` / `OpenWorld` travel with the tool.
`withCatalog` adds `search_operations` and `invoke`.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store).flatMap(_.withCatalog)).flatMap { mcp =>
            mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None))).map { out =>
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
            val call = BoxOffice.rpc(ClientRequest.CallTool(ToolName("get_show"), Json.Obj("id" -> Json.Num(1))))
            mcp.handle(call).map(_.get.toJson.contains("Evening bill"))
          }
        }
      }.assert(ok => assertTrue(ok)),
      illustrationIO(Hub.Lives.rpc).live.withMountKey(InteractiveRegistry.JsonRpcInspector),
    ),
    section("Results")(
      md"""
MCP says `structuredContent` is an object and `outputSchema` is an object schema. An output that is
an object goes as is. Anything else, whether a string, a list, or a sum type, goes as
`{"value": ...}`, and the published schema is wrapped to match, so a strict client can validate every
result. The text block carries the same JSON for a model that reads only text.

A failure the endpoint declares (`outError`, `outErrors`) is `isError: true`, never a JSON-RPC
error, and its JSON travels as `structuredContent` too, so a typed caller gets the case back:
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
            val call = BoxOffice.rpc(ClientRequest.CallTool(ToolName("get_show"), Json.Obj("id" -> Json.Num(99))))
            mcp.handle(call).map { out =>
              out
                .flatMap(_.toJson.fromJson[Json.Obj].toOption)
                .flatMap(_.get("result"))
                .flatMap(_.toJson.fromJson[CallToolResult].toOption)
            }
          }
        }
      }.assert { result =>
        val missing = result.flatMap(_.structuredContent).flatMap(_.toJson.fromJson[ShowNotFound].toOption)
        assertTrue(result.exists(_.failed), missing.contains(ShowNotFound("show 99")))
      },
      expectFail("""heddle.mcp.protocol.ToolName("get show")""").assert { errors =>
        assertTrue(errors.exists(_.message.contains("not a tool name")))
      },
    ),
    section("Resources and extensions")(
      md"""
A resource is something a client lists and reads by URI: a document, a schema, the HTML of an
MCP App view. `withResources` adds them. The server then advertises `resources` and answers
`resources/list` and `resources/read`. An unknown URI is `-32002`, and a resource that cannot be read
right now is an internal error for that one request. `withExtension` advertises an extension, such as
`ExtensionId.Ui` for MCP Apps, in both handshakes.

Adding never overwrites. A second tool with a taken name, a second resource with a taken URI, or
a catalog tool that would shadow an operation named `invoke` is a `McpBuildError`.
""",
      exampleZIO {
        val board = Resource("ui://box-office/board", "board", mimeType = Some("text/html;profile=mcp-app"))
        BoxOffice.seed.flatMap { store =>
          ZIO
            .fromEither(BoxOffice.mcpOf(store).flatMap(_.withResources(ServedResource.text(board, "<p>tonight</p>"))))
            .map(
              _.withExtension(ExtensionId.Ui, Json.Obj("mimeTypes" -> Json.Arr(Json.Str("text/html;profile=mcp-app"))))
            )
            .flatMap { mcp =>
              for
                caps <- mcp.handle(BoxOffice.rpc(ClientRequest.Discover))
                read <- mcp.handle(BoxOffice.rpc(ClientRequest.ReadResource("ui://box-office/board")))
              yield (caps.map(_.toJson).getOrElse(""), read.map(_.toJson).getOrElse(""))
            }
        }
      }.assert { case (caps, read) =>
        assertTrue(
          caps.contains("\"resources\":{}"),
          caps.contains("io.modelcontextprotocol/ui"),
          read.contains("<p>tonight</p>"),
        )
      },
    ),
    section("Calling MCP servers")(
      md"""
Heddle speaks MCP in both directions. `McpClient.http(url, settings)` connects over Streamable
HTTP with the `Client` in the environment. `McpStdio.spawn(ChildCommand(...), settings)` starts a
server as a child process on the JVM or Node and speaks over its stdio, with arguments passed
through untouched, never through a shell. `McpClient.pipe` works over any `LinePipe`. The session
tries 2026-07-28 `server/discover` first and falls back to a 2025-11-25 `initialize` session. The
scope owns the connection and the process.

`session.call(endpoint)(in)` is the typed call. It builds the tool's arguments from the same
`Endpoint` the server binds, and reads the result back with that endpoint's codecs. A declared
error comes back as `McpCallFailure.Domain(e)`:
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
            ZIO.scoped {
              McpClient
                .http("http://box-office.test/mcp", McpClient.Settings(Implementation("docs", "1")))
                .provideSome[Scope](Client.inMemory(mcp.routes))
                .flatMap { session =>
                  session.call(BoxOffice.getShow)(1) <*> session.call(BoxOffice.getShow)(99).either
                }
            }
          }
        }
      }.assert { case (show, missing) =>
        assertTrue(show.title == "Evening bill", missing == Left(McpCallFailure.Domain(ShowNotFound("show 99"))))
      },
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
