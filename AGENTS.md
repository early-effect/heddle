# AGENTS.md

Guidance for agents working in **heddle** (`early-effect/heddle`). Scala 3 / ZIO 2 HTTP library.
`histos.scala` declares the graph and the `heddle` library ship. Published coordinates remain under `rocks.earlyeffect`.

## Layout

| Path | Role |
| --- | --- |
| [`heddle/`](heddle/) | Published core. ZIO + zio-json. JVM / Loom. |
| [`brotli/`](brotli/) | Published `heddle-brotli`. RFC 7932 `br`, no JNI. |
| [`oauth/`](oauth/) | Published `heddle-oauth`. JOSE, resource server, OAuth client, OIDC provider. |
| [`mcp-protocol/`](mcp-protocol/) | Published `heddle-mcp-protocol`. MCP wire types and codecs (`Message`, `ClientRequest`, `RpcError`, `Tool`, `CallToolResult`, resources, `Structured`). zio-json only, so a browser view can depend on it. |
| [`mcp/`](mcp/) | Published `heddle-mcp`. MCP 2026-07-28 server (HTTP + stdio) over `Api` / `BoundOp`, built on `heddle-mcp-protocol`. HTTP and stdio also answer 2025-11-25 `initialize`. `server/` is the engine and envelopes; `transport/` parses bytes and picks the era; `client/` is `McpClient` (HTTP, pipe) and `McpStdio` (JVM, Node child processes). `ToolShapes` is the one place both sides read result shapes from. |
| [`mcp-apps/`](mcp-apps/) | Published MCP Apps model (`mcpApps`, JVM / JS / Native). |
| [`mcp-apps-host/`](mcp-apps-host/) | Published host adapters (`mcpAppsHost`, JVM / JS). |
| [`mcp-apps-frame/`](mcp-apps-frame/) | Published JS frame (`mcpAppsFrame`). |
| [`opentelemetry/`](opentelemetry/) | Published JVM tracing adapter. |
| [`example/`](example/) | Runnable users API + embedded OP + Swagger UI + MCP. Not in the library ship. |
| [`bench/`](bench/) | JVM benchmark mains. Manual measurement, not a throughput CI gate. |
| [`docs/`](docs/) | Specular DocSpecs + `BuildSite`, JVM half of the `docs` module. |
| [`docs-js/`](docs-js/) | JS client for `exampleDom`, JS half of the same `docs` module. No `docsJS` module. |
| [`build-tooling/`](build-tooling/) | Source plugin: relay, browser assets/check, and docs transcript. |
| [`mcp-apps-relay/`](mcp-apps-relay/) | JS relay producer (`mcpAppsRelay`), generated source consumed by `mcpAppsHost`. |
| [`browser-check/`](browser-check/) | Full JS browser link (`browser`) checked for Node dependencies. |
| [`apps-browser/`](apps-browser/) | Verify-only JVM suite plus `appsBrowserShared`, `appsBrowserView`, `appsBrowserHost`. |
| [`perfTests/`](perfTests/) | Verify-only JVM performance suite. Explicit local selection, no throughput campaign. |
| [`native-tests/`](native-tests/) | One aggregated Native test binary (`nativeTests`). |
| [`.github/workflows/ci.yml`](.github/workflows/ci.yml) | Generated workflow. Do not hand-edit. |

Histos modules are not an sbt root aggregate. `heddle`, `brotli`, `oauth`, `mcpProtocol`, `mcp`, `mcpApps`,
`mcpAppsHost`, `mcpAppsFrame`, and `opentelemetry` belong to the library ship. The remaining modules are
internal producers, examples, documentation, or verification. Generation dependencies are declared by the
build plugin: relay source into host Main, two browser assets into `appsBrowser` Test, and one transcript
into docs JVM Test and JS Main. Keep these stages separate.

Core packages under `heddle/src/main/scala/heddle/`:

| Path | Role |
| --- | --- |
| `exports.scala` | Curated `import heddle.*` facade. Datastar is not on it. |
| `http/` | Request, Response, Body, Method, Status, HttpVersion, Url, Path, QueryParams, MediaType, encodings |
| `http/header/` | HeaderName, Header, Headers, TypedHeader, Cookie, Auth, Host, dates |
| `route/` | Handler, Route, Routes, Middleware, PathCodec, PathDsl, QueryCodec, HeaderCodec. Path macros stay here. |
| `endpoint/` | Endpoint, BoundOp, Api, Schema, BodyCodec, OpenApi, OpArgs |
| `Server.scala` | `package heddle` (ZIO Tag). Do not `export Server`. |
| `server/` | Tls, Files, Compressor, Http2Config, HeddleApp |
| `client/` | Client, ClientError, CallFailure, ClientTls (JVM, Native). `client/internal/` is the shared pooled HTTP/1.1 exchange; platforms only supply a `Connector`. |
| `error/` | HeddleError, HttpError, ServerError |
| `auth/` | Basic, Bearer, API key extractors. JWT lives in `heddle-oauth`. |
| `sse/` `ws/` `datastar/` | Public subpackages. Datastar `readSignals` is an extension on Request, not a Request method. |
| `internal/` | Ascii, ConnBuf, Nio, Http1, OpenAPI printer |
| `internal/h2/` | HTTP/2 engine (`private[heddle]`) |

User JSON is `zio.json.JsonCodec` (`import heddle.*` re-exports it). `.in[A]` and `.out[A]` need a `BodyCodec[A]`. `derives Schema, JsonCodec` is the JSON default. A `given BodyCodec[A]` replaces it. `Text`, `Html`, `Javascript`, `Css`, `Svg`, `Octet`, `Form`, and `EventStream` are the other bodies. Schema (OpenAPI shapes) lives in core.

## Commands

Use the repo's pinned `./histos` launcher with JDK 25+. `test` runs selected suites in full; `--quick`
can skip unchanged suites. A plain test excludes Verify-only modules. Name `appsBrowser` or `perfTests`
explicitly when proving those lanes. Use `--platform`, not positional platform tokens.

```bash
./histos graph
./histos compile --platform jvm
./histos test --platform jvm
./histos test heddle --platform jvm
./histos test brotli oauth mcp --platform jvm
./histos test docs --platform jvm
./histos test --platform js
./histos test nativeTests --platform native
./histos test appsBrowser --platform jvm
./histos run dev.earlyeffect.specular/site
./histos '~' run dev.earlyeffect.specular/preview
./histos run oauth --platform jvm --main heddle.oauth.provider.ProviderApp
./histos run example --platform jvm --main example.Main
./histos fmt --check
./histos workflow --check
```

## CI and publishing

The Histos graph declares formatting, doc checks, JVM suites, dependency/security and workflow checks,
ship versions/compatibility, JS and Native verify jobs, `browsers`, `browser-check`, and the Specular `docs`
job. Specular builds the site and links its JS client; use its watched preview, not a static server over
`target/site`. Browser CI uses the `RunnerTools` Playwright pin and Chekhov's versioned CLI cache.

During migration, retained sbt/project/zipx files and generated setup/workflows remain until the assembled
startup, graph, and parity gates pass. They are not the operational command reference. Do not delete them
or hand-edit generated workflows. After that gate, the owning integration step runs `./histos workflow --update`
and commits its generated outputs, then proves `./histos workflow --check`.

Publishing is CI-only. Histos generates snapshot and release workflows for the ship. Snapshots follow
successful default-branch CI, eligible `snapshots`-labeled PR CI, or dispatch. Release dispatch is not authorized
until the user confirms the feature is complete. Do not cut non-snapshot tags, dispatch releases, publish,
merge PRs, or push `main` without the required approval. Consumer sbt installation snippets remain valid.

## Conventions

- Scala 3 only: `enum`, `given`, `derives`, inline, optional braces. Scalafmt: max 120, `align.preset = more`.
- `-Wunused:all`: no unused imports or locals.
- Handler is `Request => ZIO[R, E, Response]`. Middleware is `Routes => Routes` via `@@`.
- Tests: ZIO Test `ZIOSpecDefault`. JSON tests use `zio.json.JsonCodec`.
- Keep [`example/`](example/) building when you change the public API.
- Transport and decode failures are typed values (`ClientError`, `CallFailure`, `HttpError`). No `orDie`, `dieMessage`, or `throw` on paths a peer can trigger.
- Prove a socket, TLS, or interruption claim with a small failing spec (`RawServer` on the JVM, `NativeTlsCloseSpec` style on Native) before changing the code.
- No AI attribution in commits or PR text.
