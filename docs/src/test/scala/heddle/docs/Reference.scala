package heddle.docs

import heddle.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Reference extends DocSpecSuite:

  def doc = page("Reference")(
    md"""
Published artifacts, the facade, the example hub, and the knobs that are not a tutorial.
How-to lives in [The hub](the-hub.html) and [Domain is data](domain-is-data.html).
""",
    section("Modules")(
      md"""
| Artifact | Depends on | Role |
| --- | --- | --- |
| `heddle` | ZIO, zio-json | HTTP types, routes, middleware, Loom server, Schema, Endpoint, OpenAPI |
| `heddle-mcp` | heddle | MCP 2026-07-28 over `Api` / `BoundOp` (HTTP/stdio also answer 2025-11-25 `initialize`) |
| `heddle-oauth` | heddle | JOSE, resource server, OAuth client, OIDC provider |
| `heddle-brotli` | heddle | RFC 7932 `br` encoder / decoder (no JNI) |

`import heddle.*` is a curated facade over `heddle.http`, `heddle.route`, `heddle.endpoint`,
`heddle.server`, `heddle.client`, and `heddle.error`. SSE, WebSocket, and Datastar stay in their
packages. Datastar `readSignals` is an extension on `Request`.
"""
    ),
    section("Example hub")(
      md"""
```bash
sbt example/run
```

- http://localhost:8080/docs — box office API + Swagger, Authorize against the embedded OP
- http://localhost:8080/preview — box office UI (same `Api`)
- http://localhost:8080/mcp — Streamable HTTP MCP (`POST`; 2026 native, 2025 `initialize`)
- `sbt "example/run -- --mcp-stdio"` — same `Api`, stdio JSON-RPC

Seed users: `ada` / `ada`. Machine client `machine` / `secret`.
"""
    ),
    section("Server config")(
      md"""
`zio.Config` keys nest under `heddle.server`. Sizes are `BytesLength` (`64.K`, `10.M`).
Forever is `Duration.Infinity`. Why each clock exists: [What stays open](what-stays-open.html).

| Field | Default | Why |
| --- | --- | --- |
| `host` | `0.0.0.0` | Bind address |
| `port` | 8080 | Bind port |
| `maxHeaderBytes` | `64.K` | Cap on the header block |
| `maxBodyBytes` | `10.M` | Cap on the request body |
| `chunkSize` | `8.K` | Read/write chunk |
| `gracefulShutdownTimeout` | 10s | Finish in-flight work, then close |
| `idleTimeout` | 60s | Close a quiet keep-alive |
| `headerTimeout` | 30s | Close incomplete headers |
| `maxConnections` | 1024 | Cap on accepted sockets |
| `maxRequestsPerConnection` | 10000 | Bound keep-alive reuse |
| `soBacklog` | 100 | Kernel accept queue |
| `reuseAddress` | true | `SO_REUSEADDR` |
| `tcpNoDelay` | true | `TCP_NODELAY` |
| `soKeepAlive` | true | Dead-peer cleanup |
| `http2` | true | Offer HTTP/2 |
| `http2Config.maxConcurrentStreams` | 100 | Cap streams per connection |
| `http2Config.initialWindowSize` | 65535 | Stream flow-control window |
| `http2Config.maxFrameSize` | `16.K` | Frame cap |
| `http2Config.maxHeaderListSize` | `8.K` | HPACK list cap |
| `http2Config.maxOutstandingFrames` | 64 | Bound writer and body queues |

```scala
Server.serve(app).provide(Server.Config.defaults)
Server.serve(app, Server.Config.default.copy(port = 8080, maxBodyBytes = 1.M, idleTimeout = Duration.Infinity))
Server.serve(app).provide(Server.Config.layer)
```

Gzip is not a field. Wrap the `Routes`: `app @@ Middleware.compress()`.

JDK 21+ (Loom). `HeddleApp` is the `ZIOAppDefault` that serves `routes` and exits 0 on Ctrl-C
under sbt 2.
""",
      exampleValue {
        (
          Server.Config.default.port,
          Server.Config.default.idleTimeout,
          Server.Config.default.maxConnections,
        )
      }.assert { case (port, idle, max) =>
        assertTrue(port == 8080, idle == 60.seconds, max == 1024)
      },
    ),
    section("Client config")(
      md"""
`zio.Config` keys nest under `heddle.client`. `Client.get` / `Client.request` / `Client.sse`
take `Client.Config` (the default is `Client.Config.default`). `Client.live` uses that default
on the pooled client.

| Field | Default | Why |
| --- | --- | --- |
| `maxConnectionsPerHost` | 10 | In-flight cap per host |
| `maxIdlePerHost` | 10 | Idle sockets kept |
| `connectTimeout` | 10s | Give up connecting |
| `idleTimeout` | 60s | Give up reading |
| `poolIdleTimeout` | 60s | Drop idle pooled sockets |
| `addUserAgent` | true | Add `User-Agent: heddle` if missing |
| `maxHeaderBytes` | `64.K` | Cap on response headers |
| `maxBodyBytes` | `10.M` | Cap on response body |

`Accept-Encoding` is not added for you. Set the header if you want gzip. The client inflates
only when that request asked for gzip.
""",
      exampleValue {
        Client.Config.default.connectTimeout
      }.assert(t => assertTrue(t == 10.seconds)),
    ),
    section("Middleware")(
      md"""
| Middleware | Role |
| --- | --- |
| `compress` | Response `Content-Encoding` on the wrapped `Routes` |
| `decompress(maxBytes)` | Inflate a request body, capped |
| `timeout` | Slow handler becomes 504. Does not close the socket |
| `cors` / `requestId` / `requireTls` | Policy on the wrapped `Routes` |
""",
      exampleValue {
        true
      }.assert(ok => assertTrue(ok)),
    ),
  )
end Reference
