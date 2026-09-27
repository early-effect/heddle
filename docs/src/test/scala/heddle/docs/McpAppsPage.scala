package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.mcp.apps.*
import heddle.mcp.client.McpClient
import heddle.mcp.protocol.{Implementation, ResourceContents}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object McpAppsPage extends DocSpecSuite:
  private val bill = Shed(UiUri("ui://box-office/bill"), "Tonight's bill", Grant.launch(BoxOffice.listShows))(
    (show = Grant.app(BoxOffice.getShow))
  )

  private val view = UiDocument("Tonight's bill", "document.getElementById('app').textContent = 'loading'")

  def doc = page("MCP Apps")(
    md"""
An MCP App attaches a view to a tool. The server serves the view's HTML as a `ui://` resource.
A host shows the tool's result in that view, inside a sandboxed iframe, and relays what the view
asks for over `postMessage`. Heddle's rule for all of it:

> The view is untrusted. The shed is typed. The host decides.

```mermaid
flowchart LR
  subgraph Server
    E[Endpoints] --> T[tools with _meta.ui]
    E --> R[ui:// view]
  end
  subgraph Host
    P[policy clamp] --> F[sandboxed iframe]
  end
  subgraph View
    V[Scala.js view] --> B[bridge]
  end
  R -- resources/read --> P
  B -- tools/call, gated --> T
```

The view never names a tool with a string. It picks a grant from its **shed**, and every grant is
one of the same `Endpoint`s the server binds.
""",
    section("The shed")(
      md"""
A shed is one view's grants and the policy it asks for. `Grant.launch` is the tool the model
calls to open the view. `Grant.app` tools are the ones only the view calls, and the host keeps
them from the model. The tools are a named tuple, so a view calls `_.show`, never `"get_show"`.
""",
      exampleValue(bill.grants.map(g => g.toolName -> g.visibility))
        .assert(gs => assertTrue(gs == List("list_shows" -> Visibility.ModelAndApp, "get_show" -> Visibility.App))),
      expectFail("""Shed(UiUri("ui://box-office/bill"), "Bill", Grant.launch(BoxOffice.listShows))((show = 42))""")
        .assert(errors => assertTrue(errors.exists(_.message.contains("must be a Grant")))),
      expectFail("""UiUri("https://box-office.example/bill")""")
        .assert(errors => assertTrue(errors.exists(_.message.contains("not a ui://")))),
    ),
    section("Serving a view")(
      md"""
`mcp.withApp(shed, document)` does the server's part:

- serves the document as the `ui://` resource, with MIME `text/html;profile=mcp-app`
- marks every granted tool with `_meta.ui` (the view's URI and who may call it)
- lists the view-only tools, which are not promoted to the model, with `visibility: ["app"]`
- advertises the MCP Apps extension

`UiDocument` renders the HTML from the view's linked Scala.js script, so the server knows the
script's CSP hash by construction and publishes it for a host to pin. Read back through
Heddle's own client, exactly as a host would:
""",
      exampleZIO {
        ZIO.scoped {
          for
            store <- BoxOffice.seed
            mcp   <- ZIO.fromEither(BoxOffice.mcpOf(store))
            app   <- ZIO.fromEither(mcp.withApp(bill, view))
            host  <- McpClient
              .http("http://box-office.test/mcp", McpClient.Settings(Implementation("docs", "1")))
              .provideSome[Scope](Client.inMemory(app.routes))
            tools    <- host.listTools
            contents <- host.readResource(bill.uri.value)
          yield
            val marked = tools.map(t => t.name.value -> UiMeta.decodeTool(t.meta)._1.visibility).toMap
            val html   = contents.collectFirst { case t: ResourceContents.Text => t.text }
            (marked, html.exists(_.contains("<div id=\"app\"></div>")))
        }
      }.assert { (marked, served) =>
        assertTrue(
          marked.get("list_shows").contains(Visibility.ModelAndApp),
          marked.get("get_show").contains(Visibility.App),
          served,
        )
      },
    ),
    section("What a view may ask for")(
      md"""
A view asks for a `UiPolicy`: which origins it reaches for each network directive, browser
permissions, a stable origin, and whether it wants a border. `UiPolicy.closed` is the default:
no network, no permissions, a fresh opaque origin. A host allows a `HostPolicy`, and the view
gets the ask **clamped** by the allowance. The clamp only takes away, and `narrowed` says what it
took so the host can log it:
""",
      exampleValue {
        val weather = Origin("https://api.weather.example")
        val tracker = Origin("https://tracker.example")
        val ask     = UiPolicy(Network(connect = Set(weather, tracker)), permissions = Set(Permission.Camera))
        val host    = HostPolicy(
          NetworkAllowance(Admit.Only(Set(weather)), Admit.none, Admit.none, Admit.none),
          permissions = Set.empty,
          stable = StableOrigins.Refuse,
        )
        val clamp = Clamp[UiPolicy, HostPolicy]
        (clamp.clamp(ask, host), clamp.narrowed(ask, host))
      }.assert { (granted, taken) =>
        assertTrue(
          granted.network.connect == Set(Origin("https://api.weather.example")),
          granted.permissions.isEmpty,
          taken.length == 2,
        )
      },
      md"""
The clamp's laws are checked over generated policies:

| Law | Meaning |
| --- | --- |
| Only narrows | The result never grants more than the ask |
| Idempotent | Clamping twice by one allowance is clamping once |
| Identity and zero | `HostPolicy.open` changes nothing; `HostPolicy.closed` leaves an isolated view |
| Monotone | A wider allowance never yields a narrower result |
| Honest audit | `narrowed` is empty exactly when nothing was taken |
""",
    ),
    section("Reading any server's _meta.ui")(
      md"""
A host reads `_meta.ui` from servers it does not control, so decoding is total and never
permissive. What it cannot trust, it drops and reports: a wildcard origin, a path, an unknown
permission, a bad `ui://` URI. It also reads the legacy `ui/resourceUri` key, as the spec asks.
""",
      exampleZIO {
        val meta =
          """{"ui":{"csp":{"connectDomains":["https://api.example.com","https://*.cdn.example.com"]},"permissions":{"camera":{},"teleport":{}}}}"""
        ZIO.fromEither(meta.fromJson[Json.Obj]).map(obj => UiMeta.decodeResource(Some(obj)))
      }.assert { (policy, problems) =>
        assertTrue(
          policy.network.connect == Set(Origin("https://api.example.com")),
          policy.permissions == Set(Permission.Camera),
          problems.length == 2,
        )
      },
    ),
    section("The view protocol")(
      md"""
A view and its host speak JSON-RPC over `postMessage`: the SEP-1865 `ui/*` methods plus MCP's
own `tools/call`, `resources/read`, and `ping`. Heddle models each direction as a closed enum,
so a message is a value, and one heddle does not know is a typed refusal:

| Enum | Direction | Messages |
| --- | --- | --- |
| `ViewRequest` | view to host | `Initialize`, `OpenLink`, `DownloadFile`, `SendMessage`, `RequestDisplayMode`, `UpdateModelContext`, `CallTool`, `ReadResource`, `Ping` |
| `ViewNotification` | view to host | `Initialized`, `SizeChanged`, `RequestTeardown`, `Log` |
| `HostNotification` | host to view | `ToolInput`, `ToolInputPartial`, `ToolResult`, `ToolCancelled`, `HostContextChanged` |
| `HostRequest` | host to view | `ResourceTeardown` |
| `SandboxMessage` | host and relay only | `ProxyReady`, `ResourceReady` |

Each encodes to a JSON-RPC `Message` and decodes back from one. A `ui/message` must come from the
user, a download may carry only resources, and a tool name must be a tool name; anything else is
`UiError.BadParams` naming the method and the reason.

The host context arrives whole in the `ui/initialize` result, then in parts: a
`host-context-changed` notification names only what changed, and `merge` applies it. The standard
theme variables are the typed `HostVar` enum, container sizes are an `Extent` per side (fixed,
bounded, or free), and fields a newer host sends that this revision does not know are kept in
`extra`.
""",
      exampleValue {
        val start = """{"theme":"light","displayMode":"inline","containerDimensions":{"width":400,"maxHeight":600}}"""
        val patch = """{"theme":"dark","styles":{"variables":{"--color-text-primary":"#fafafa"}}}"""
        for
          context <- start.fromJson[heddle.mcp.apps.ui.HostContext]
          change  <- patch.fromJson[heddle.mcp.apps.ui.HostContext]
        yield context.merge(change)
      }.assert { merged =>
        import heddle.mcp.apps.ui.*
        assertTrue(
          merged.map(_.theme) == Right(Some(Theme.Dark)),
          merged.map(_.displayMode) == Right(Some(DisplayMode.Inline)),
          merged.map(_.containerDimensions) == Right(Some(ContainerDimensions(Extent.AtMost(600), Extent.Fixed(400)))),
          merged.map(_.styles.map(_.variables)) == Right(Some(Map(HostVar.ColorTextPrimary -> "#fafafa"))),
        )
      },
      exampleValue {
        import heddle.mcp.apps.ui.*
        ViewRequest.decode("ui/message", Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Arr()))
      }.assert { refused =>
        assertTrue(refused.left.toOption.map(_.message).contains("ui/message: role must be user, not assistant"))
      },
    ),
    section("The view bridge")(
      md"""
A view connects to its host with `AppBridge.connect(shed, port, settings)`. The handshake is
`ui/initialize` and then `ui/notifications/initialized`; the bridge answers and routes messages
until its scope closes. `ViewPort` is the transport. In a browser it is
`PostMessageBridge.toParent`: the view posts to `window.parent` and hears only messages from it,
as the ext-apps SDK does, and anything that is not JSON-RPC is dropped. `ViewPort.pair` joins a
host and a view in one process, as the tests do.

```scala
ZIO.scoped {
  for
    bridge <- AppBridge.connect(bill, PostMessageBridge.toParent, AppBridge.Settings(Implementation("bill-view", "1")))
    show   <- bridge.call(_.show)(7)          // Int in, Show out, ShowNotFound as a typed error
  yield show
}
```

A view calls only its shed's grants, by picking them, and each call reuses the MCP client's
typed path: the input becomes the tool's arguments, and the result comes back as the endpoint's
output or one of its declared errors (`McpCallFailure.Domain`).

The view renders the launch tool's lifecycle from `bridge.run`, a stream of `Run[In, Err, Out]`
whose types come from the shed's launch grant. The host's notifications step it: a partial input
the model is still writing (shown, never trusted), the call, and its result, cancellation, or a
payload the launch tool's types cannot read. The host context arrives with the handshake and
changes through `bridge.context`. When the host tears the view down, the bridge runs the view's
`onTeardown` before it answers.
""",
      exampleZIO {
        import heddle.mcp.apps.ui.*
        for
          called <- Run.step(BoxOffice.listShows)(Run.waiting, HostNotification.ToolInput(Json.Obj()))
          early  <- Run.step(BoxOffice.listShows)(
            Run.waiting,
            HostNotification.ToolResult(heddle.mcp.protocol.CallToolResult(Chunk.empty)),
          )
        yield (called, early)
      }.assert { (called, early) =>
        import heddle.mcp.apps.ui.*
        assertTrue(
          called == Run.Called(()),
          early == Run.Unreadable("a tool result arrived before its input"),
        )
      },
    ),
  )
end McpAppsPage
