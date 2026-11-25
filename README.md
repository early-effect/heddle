# Heddle

One BoundOp. HTTP, OpenAPI, and MCP are hosts.

Docs: [https://www.earlyeffect.rocks/heddle/](https://www.earlyeffect.rocks/heddle/)

`import heddle.*` is a curated facade over `heddle.http`, `heddle.route`, `heddle.endpoint`,
`heddle.server`, `heddle.client`, and `heddle.error`. SSE, WebSocket, and Datastar stay in
their own packages.

## Install

Core (ZIO only):

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "0.11.0"
```

Optional artifacts, same version: `heddle-mcp`, `heddle-mcp-protocol`, `heddle-mcp-apps`, `heddle-oauth`,
`heddle-brotli`.

Cross-build: `%%` is JVM. Use `%%%` for Scala.js and Scala Native. `heddle`, `heddle-mcp`,
`heddle-mcp-protocol`, and `heddle-mcp-apps` publish on all three. `heddle-oauth` is JVM and JS.
`HeddleApp` is JVM-only.

## Routes

```scala
import heddle.*
import zio.*

val routes = Routes(
  Method.GET / "health"            -> Handler.text("ok"),
  Method.GET / "users" / int("id") -> { id =>
    ZIO.succeed(Response.text(id.toString))
  },
)

val app = routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug)

object Hello extends HeddleApp:
  def routes = app
```

`id` is the name in `int("id")`. The parameter name is the capture name.

Promote selected endpoints with `Api.job` (or `.mcp`), then `Mcp.from(api)`. Agents hit `POST /mcp` or a
`--mcp-stdio` process. Swagger is `api.openApi.routes("docs")`. A browser is an HTTP client
of `api.routes`.

## Run the example hub

```bash
./histos run example --platform jvm --main example.Main  # HTTP + OP + Swagger at /docs, UI at /preview, MCP at /mcp
./histos run example --platform jvm --main example.Main -- --mcp-stdio  # same Api on stdio
```

Grok Build against the running process:

```toml
[mcp_servers.heddle-example]
url = "http://localhost:8080/mcp"
```

## Modules

| Artifact | Depends on | Role |
| --- | --- | --- |
| `heddle` | ZIO, zio-json, neotype | HTTP types, routes, middleware, `Server.install` (JVM, JS, Native), Schema, Endpoint, OpenAPI |
| `heddle-mcp-protocol` | zio-json | MCP wire types and codecs: JSON-RPC messages, requests, tools, results, resources |
| `heddle-mcp` | heddle, heddle-mcp-protocol | MCP 2026-07-28 over `Api` / `BoundOp` (HTTP/stdio also answer 2025-11-25 `initialize`). Events: poll and signed webhooks |
| `heddle-mcp-apps` | heddle-mcp | MCP Apps: sheds of typed grants, `ui://` views, `_meta.ui`, and the policy a host clamps |
| `heddle-mcp-apps-host` | heddle-mcp-apps | JVM / JS host adapters |
| `heddle-mcp-apps-frame` | heddle-mcp-apps-host | JS frame adapter |
| `heddle-oauth` | heddle | JOSE, resource server, OAuth client, OIDC provider |
| `heddle-brotli` | heddle | RFC 7932 `br` encoder / decoder (no JNI) |
| `heddle-opentelemetry` | heddle, zio-opentelemetry | JVM tracing adapter |

The library runs on JDK 21+. JS is Node. Native is POSIX plus OpenSSL. `HeddleApp` is JVM-only.
Full guide: [earlyeffect.rocks/heddle](https://www.earlyeffect.rocks/heddle/).

## Develop this repository

The pinned `./histos` launcher needs JDK 25+. The sbt installation snippets above are consumer code,
not commands for building this checkout. `histos.scala` keeps the published library ship separate from
examples, benchmark mains, documentation, generated-source producers, and verification modules.
`docs` has JVM DocSpecs and the JS client from `docs-js`, not a separate `docsJS` module.

```bash
./histos compile --platform jvm
./histos test --platform jvm                    # full suites, excludes Verify-only lanes
./histos test --platform js
./histos test nativeTests --platform native    # one aggregated Native binary
./histos test appsBrowser --platform jvm       # explicit browser verification
./histos run dev.earlyeffect.specular/site
./histos '~' run dev.earlyeffect.specular/preview
./histos fmt --check
./histos workflow --check
```

The source build plugin stages the relay into host Main, browser view/host JS assets into browser Test,
and the transcript into docs JVM Test and JS Main. CI includes JVM tests, JS and Native verification,
`browsers`, `browser-check`, formatting, doc/workflow/security checks, ship compatibility, and Specular docs.
Histos is the repository's only build. Regenerate CI from the typed graph with `./histos workflow --update`,
then prove `./histos workflow --check` and commit the generated files. Do not hand-edit generated YAML.
The setup action is pinned to the published SDK's source commit; `build.properties` selects the toolchain.
The repository's `.jvmopts` still supplies launcher JVM options. Install the repository hooks with
`scripts/install-git-hooks` to use the same `./histos fmt --check` gate as CI.

Publishing is CI-only. Snapshot integration precedes completion, through successful default-branch CI,
eligible `snapshots`-labeled same-repository PR CI, or snapshot dispatch. Fork PRs cannot enter the
credential-bearing snapshot job. Docs deploy after verified default-branch CI; `histos-pages.yml` also
provides manual Pages dispatch. A non-snapshot release waits for explicit feature completion approval.
Nothing in a local build authorizes publication or merging.

### Manual benchmark and profile scripts

Run `scripts/bench-heddle.sh` and `scripts/bench-zio-http.sh` separately. The corresponding
`bench-safety-*` scripts retain the safety probe, not a request-rate comparison. `scripts/profile-heddle.sh`
records a 30s JFR and prints the optional `asprof` command while the owned server is alive.
`PORT`, `DURATION`, `CONNECTIONS`, and profile `OUT` retain their existing meanings.

Bench `JvmOptions` supply `UnlockDiagnosticVMOptions` and `DebugNonSafepoints` to the actual JVM child.
Scripts default `JAVA_OPTS` to `-Xms1g -Xmx1g` and append it to inherited `JAVA_TOOL_OPTIONS`, which the JVM
parses without shell evaluation. Custom heap, stack, and `-D` options therefore still reach the application.
This is a JVM-options override, not shell syntax or a classpath/main selector; no `eval` is used.
The typed diagnostics remain enabled even with custom options. A pre-existing listener is rejected, never killed.
The atomic `/tmp/heddle-bench.lock.d` records its owner; remove a stale lock only after checking that owner.

Histos browser CI installs its pinned Playwright CLI in Chekhov's versioned cache. On Linux,
`scripts/install-browser-deps.sh` uses that exact pin (or a matching `PLAYWRIGHT_DRIVER_CLI`) for system libraries;
it does not pick an arbitrary cached version or install a package. That script needs explicit local sudo permission.
