package heddle.docs

import heddle.{Api, Body, Client, Endpoint, LinePipe, Observed, OpArgsError, Request, Schema, SseField}
import heddle.docs.fixture.*
import heddle.docs.ui.Hub
import heddle.mcp.{Mcp, McpBuildError, ServedResource, Session}
import heddle.mcp.client.{McpCallFailure, McpClient, McpError}
import heddle.mcp.events.{TtlPolicy, VerificationPolicy}
import heddle.mcp.protocol.{
  CallToolResult,
  ClientRequest,
  ContentBlock,
  ExtensionId,
  Implementation,
  Message,
  Methods,
  ProtocolVersion,
  RequestId,
  Resource,
  RpcError,
  ToolName,
}
import heddle.mcp.transport.Http as McpHttp
import heddle.sse.SseCodec
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
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
            mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None))).some.map { out =>
              val json = out.toJson
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
            mcp.handle(call).some.map(_.toJson.contains("Evening bill"))
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
MCP App view. `withResources` adds them. The server then advertises `resources`, including
`subscribe`, and answers `resources/list` and `resources/read`. An unknown URI is `-32002`, and a resource that cannot be read
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
          caps.contains("\"resources\":{\"subscribe\":true}"),
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
    section("While a tool is running")(
      md"""
A tool calls `Session.publish` and does not name the pipe or HTTP. That notification is one whole
message ahead of the tool's result: the next stdout line, or an SSE event on the POST when
`Accept` contains `text/event-stream`. `McpClient.http` sends that Accept. Without the media type,
and outside a request, `publish` succeeds and sends nothing.

The next request runs while the tool is still waiting. Match answers by JSON-RPC id. A later call
can finish first.

`open` keeps the call. `notifications` on that call are what the reader recorded before `result`
completed. `cancel` sends `notifications/cancelled` and fails `result` with `McpError.Cancelled`,
so the waiter does not sit until the session timeout. The server writes no result for a call
interrupted before that result. Another call on the same session keeps running. `request` is
`open` plus the timeout.

`initialize` is answered before the next line is read, and it cannot be cancelled. Interrupting
`stdio` interrupts in-flight tools. End of stdin does not: the server finishes what it has already
read and writes those results.

GET, with a session id and `Accept: text/event-stream`, is that session's resource stream. Tool
notifications do not travel on it. A stateless server has one request-id space, so two clients
that both use id `1` can cancel each other. A session id keeps the ids in that session.
""",
      exampleZIO {
        final case class Arg(name: String) derives Schema, JsonCodec
        val tickNote: Message.Notification =
          Message.Notification("notifications/progress", Json.Obj("token" -> Json.Str("tick")))
        val settings                         = McpClient.Settings(Implementation("docs", "1"))
        def textOf(result: Json.Obj): String =
          result.as[CallToolResult] match
            case Right(got) => got.content.collect { case ContentBlock.Text(text, _) => text }.mkString
            case Left(_)    => ""

        for
          started <- Promise.make[Nothing, Unit]
          stopped <- Promise.make[Nothing, Unit]
          gate    <- Promise.make[Nothing, Unit]
          mcp     <- ZIO.fromEither(
            Mcp
              .from(Api("shop", "1"))
              .flatMap(_.tool[Arg]("hold") { a =>
                (started.succeed(()) *> gate.await).onInterrupt(stopped.succeed(())).as(a.name)
              })
              .flatMap(_.tool[Arg]("pass")(a => ZIO.succeed(a.name)))
              .flatMap(_.tool[Arg]("tick")(_ => Session.publish(tickNote) *> ZIO.succeed("done")))
          )
          (client, remote) <- LinePipe.connected
          _                <- mcp.stdio(remote).forkScoped
          session          <- McpClient.pipe(client, settings)
          held      <- session.open(ClientRequest.CallTool(ToolName("hold"), Json.Obj("name" -> Json.Str("ada"))))
          _         <- started.await
          passed    <- session.open(ClientRequest.CallTool(ToolName("pass"), Json.Obj("name" -> Json.Str("bea"))))
          passText  <- passed.result.map(textOf)
          _         <- held.cancel
          _         <- stopped.await
          cancelled <- held.result.flip
          ticked    <- session.open(ClientRequest.CallTool(ToolName("tick"), Json.Obj("name" -> Json.Str("ada"))))
          tickText  <- ticked.result.map(textOf)
          notes     <- ticked.notifications
        yield (passText, cancelled, notes == Chunk(tickNote), tickText)
      }.assert { case (passText, cancelled, sameNote, tickText) =>
        val wasCancelled = cancelled match
          case McpError.Cancelled(_) => true
          case _                     => false
        assertTrue(passText == "bea", wasCancelled, sameNote, tickText == "done")
      },
      exampleZIO {
        final case class Arg(name: String) derives Schema, JsonCodec
        val tickNote: Message.Notification =
          Message.Notification("notifications/progress", Json.Obj("token" -> Json.Str("tick")))
        val args = Json.Obj("name" -> Json.Str("ada"))
        for
          mcp <- ZIO.fromEither(
            Mcp
              .from(Api("shop", "1"))
              .flatMap(_.tool[Arg]("tick")(_ => Session.publish(tickNote) *> ZIO.succeed("done")))
          )
          msg = Message.stateless(RequestId.Num(1), ClientRequest.CallTool(ToolName("tick"), args))
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(msg.json.toJson))
              .withHeader(McpHttp.ProtocolHeader, ProtocolVersion.Current.value)
              .withHeader(McpHttp.MethodHeader, "tools/call")
              .withHeader(McpHttp.NameHeader, "tick")
              .withHeader("Accept", "application/json, text/event-stream")
          )
          events <- SseCodec.stream(res.body.toStream).runCollect
        yield events.map(_.data)
      }.assert { data =>
        assertTrue(
          data.size == 2,
          data.lift(0).exists(_.contains("notifications/progress")),
          data.lift(1).exists(_.contains("done")),
        )
      },
    ),
    section("A stream is not a tool")(
      md"""
`.stream` is a subscription. `tools/list` does not name it, and `tools/call` cannot run it.
`server/discover` lists it under `subscriptions`, with the operation's input schema and the element
schema. `subscriptions/listen` is one pull. The acknowledgement comes first, then one
`notifications/operation/event` per element, then a complete result. The request id is the
subscription id. The element's id is the application's resume id.

`.mcp` marks an operation as a tool. `.mcp.stream` still carries that mark, and `Mcp.from` refuses
it (`OpArgsError.Subscription`). An HTTP-only stream stays off the `Api` passed to `Mcp.from`.
Closing the listen body, or `notifications/cancelled` for that request id, interrupts that pull
only. The operation keeps running for everyone else.
""",
      exampleZIO {
        final case class Tick(n: Int) derives Schema, JsonCodec
        val ticks     = Endpoint.get("ticks").stream[Tick]
        val promoted  = Endpoint.get("ticks").mcp.stream[Tick]
        val arguments = Json.Obj()
        val listen    = Message.stateless(RequestId.Num(3), ClientRequest.Listen(ToolName("get_ticks"), arguments))
        for
          server <- ZIO.fromEither(
            Mcp.from(
              Api("ticks", "1").stream(ticks)(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(7), Tick(7)))))
            )
          )
          listed <- server.handle(Message.stateless(RequestId.Num(1), ClientRequest.ListTools(None)).json)
          found  <- server.handle(Message.stateless(RequestId.Num(2), ClientRequest.Discover).json)
          res    <- server.routes(
            Request
              .post("/mcp", Body.json(listen.json.toJson))
              .withHeader(McpHttp.ProtocolHeader, ProtocolVersion.Current.value)
              .withHeader(McpHttp.MethodHeader, "subscriptions/listen")
              .withHeader("Accept", "text/event-stream")
          )
          events <- SseCodec.stream(res.body.toStream).runCollect
        yield
          val tools    = listed.fold("")(_.toJson)
          val discover = found.fold("")(_.toJson)
          val wire     = events.map(_.data).mkString("\n")
          val refused  = Mcp.from(Api("ticks", "1").stream(promoted)(_ => ZIO.succeed(ZStream.empty))) match
            case Left(errors) =>
              errors.exists {
                case McpBuildError.NotPromotable("get_ticks", OpArgsError.Subscription) => true
                case _                                                                  => false
              }
            case Right(_) => false
          (tools.contains("get_ticks"), discover, wire, refused)
      }.assert { case (inTools, discover, wire, refused) =>
        assertTrue(
          !inTools,
          discover.contains("\"subscriptions\""),
          discover.contains("get_ticks"),
          discover.contains("elementSchema"),
          wire.contains("notifications/subscriptions/acknowledged"),
          wire.contains("notifications/operation/event"),
          wire.contains(""""id":"7""""),
          wire.contains(""""n":7"""),
          wire.contains("\"resultType\":\"complete\""),
          refused,
        )
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
| Session | none. One request-id space for the process. | HTTP mints an `Mcp-Session-Id`. Cancel and GET use it. stdio is one pipe and has no session header. |
| HTTP | POST. A tool call is an event stream when Accept includes `text/event-stream`. | POST the same way. GET with a session id and `text/event-stream` is the resource stream. DELETE closes the session. |
| stdio | `_meta` on every line. Requests overlap. | `initialize`, then the rest of that process is 2025. Requests overlap. |

`initialize` selects compat. `MCP-Protocol-Version: 2026-07-28`, `_meta` version `2026-07-28`, or
`server/discover` selects native. A later POST that carries a live session id and omits
`MCP-Protocol-Version` stays on that session. A POST with neither a session id nor the protocol
header is still an error, and a protocol header that names any other revision is rejected.

Tool notifications travel on the request that started the tool. GET is the resource stream for
that session, not a second path for those lines. Do not grow it into "the" notification channel.
"""
    ),
    section("HTTP and stdio")(
      md"""
`mcp.routes` is Streamable HTTP at `/mcp` (`mcp.at("other")` to move it). `mcp.stdio()` reads
JSON-RPC lines from stdin and writes lines to stdout. Both keep reading while a tool runs. See
"While a tool is running" for `Session.publish`, overlapping calls, and cancel. Both transports
speak native 2026 and the 2025 handshake.

Do not embed a second authorization server inside `heddle-mcp`. When the HTTP transport needs a
bearer token, `Mcp.bearer` + `Mcp.protectedResource` advertise the resource metadata. JWT
verification lives in `heddle-oauth`.
"""
    ),
    section("Events")(
      md"""
`io.modelcontextprotocol/events` is advertised once the server declares an event. It is a
2026-07-28 method: a 2025 session gets method not found. `poll` and `webhook` are the modes this
server implements. Push is refused.

Declare the event, then choose the two policies the way you choose any other server setting.
`eventPolicy` sets `TtlPolicy`: `floor` raises a short `ttlMs`, an omitted `ttlMs` uses `default`,
and `cap` bounds a longer one, including `ttlMs: null`. `verificationPolicy` chooses the subscribe
challenge. `VerificationPolicy.Required` (the default) posts a signed nonce and commits only when
the `2xx` body echoes it. `VerificationPolicy.Skip` commits that first subscribe with no challenge
POST, for a hosted receiver that answers with its own body.

`delivery.headers` is a heddle extension. The events spec does not define it. An optional object of
string to string is copied onto every webhook POST, including the verification challenge. A
receiver that checks `Authorization: Bearer <key>` puts that header here. The signature still
covers only the id, the timestamp, and the body.

Names are rejected, case-insensitively, when they are hop-by-hop (`Host`, `Content-Length`,
`Content-Type`, `Transfer-Encoding`, `Connection`, and the rest of that set), when they start with
`webhook-` or `X-MCP-`, or when the block exceeds 16 names or 8192 characters. A value is at most
4096 characters and cannot contain a control character. Values are secrets: they are absent from
`toString`, from the subscribe result, and from the error text. They are not part of the
subscription id. Subscribing again with the same principal, url, name, and arguments replaces the
headers on that subscription.

The signed POST itself is JVM-only. JS and Native fail the send with `Unavailable`.
""",
      exampleValue {
        case class Filter(kind: String) derives Schema, JsonCodec
        case class Milestone(kind: String, note: String) derives Schema, JsonCodec
        val built = Mcp
          .from(Api("Shop", "1"))
          .flatMap {
            _.event[Filter, Milestone]("session.milestone", "A session milestone").pollAndWebhook((filter, milestone) =>
              filter.kind == milestone.kind
            )
          }
          .map(
            _.eventPolicy(
              TtlPolicy.default.copy(
                floor = Duration.fromMillis(60_000),
                default = Duration.fromMillis(900_000),
                cap = Duration.fromMillis(3_600_000),
              )
            ).verificationPolicy(VerificationPolicy.Skip)
          )
        built.isRight
      }.assert(ok => assertTrue(ok)),
      exampleValue {
        val token = "Bearer receiver-key"
        val base  = Chunk(
          "mode"   -> Json.Str("webhook"),
          "url"    -> Json.Str("https://hooks.example/events"),
          "secret" -> Json.Str("whsec_placeholder"),
        )
        def params(headers: Json.Obj) = Json.Obj(
          "name"      -> Json.Str("session.milestone"),
          "arguments" -> Json.Obj("kind" -> Json.Str("edit")),
          "delivery"  -> Json.Obj(base :+ ("headers" -> headers)),
        )
        val decoded = ClientRequest.decode(
          Methods.SubscribeEvents,
          params(Json.Obj("Authorization" -> Json.Str(token))),
        )
        val refused = ClientRequest.decode(Methods.SubscribeEvents, params(Json.Obj("Host" -> Json.Str(token))))
        val carried = decoded.toOption match
          case Some(ClientRequest.SubscribeEvents(_, _, _, _, _, _, _, headers)) =>
            headers.entries == Chunk("Authorization" -> token) && !decoded
              .fold(_.text, _.toString)
              .contains(
                "receiver-key"
              )
          case _ => false
        (carried, refused)
      }.assert { (carried, refused) =>
        assertTrue(
          carried,
          refused == Left(RpcError.InvalidParams("delivery.headers rejects Host")),
          !refused.fold(_.text, _.toString).contains("receiver-key"),
        )
      },
    ),
  )
end Agents
