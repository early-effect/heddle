package heddle.docs

import heddle.*
import specular.*
import specular.ziotest.DocSpecSuite
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
| `heddle` | ZIO | HTTP types, routes, middleware, Loom server, Schema, Endpoint, OpenAPI |
| `heddle-zio-json` | heddle, zio-json | `given JsonCodec[A]` from zio-json |
| `heddle-mcp` | heddle, heddle-zio-json | MCP 2026-07-28 over `Api` / `BoundOp` |
| `heddle-oauth` | heddle, heddle-zio-json | JOSE, resource server, OAuth client, OIDC provider |
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

- http://localhost:8080/docs — directory API + Swagger, Authorize against the embedded OP
- http://localhost:8080/preview — directory UI (same `Api`)
- http://localhost:8080/mcp — Streamable HTTP MCP (`POST`)
- `sbt "example/run -- --mcp-stdio"` — same `Api`, stdio JSON-RPC

Seed users: `ada` / `ada`. Machine client `machine` / `secret`.
"""
    ),
    section("Server config")(
      md"""
`Server.Config` defaults: host `0.0.0.0`, port 8080, `maxHeaderBytes` 64 KiB, `maxBodyBytes` 10 MiB,
`chunkSize` 8 KiB, `gracefulShutdownTimeout` 10s, HTTP/2 on.

```scala
Server.serve(app).provide(Server.Config.defaults)
Server.serve(app, Server.Config.default.copy(port = 8080, maxBodyBytes = 1.M))
Server.serve(app).provide(Server.Config.layer) // heddle.server.host, heddle.server.port, ...
```

JDK 21+ (Loom). `HeddleApp` is the `ZIOAppDefault` that serves `routes` and exits 0 on Ctrl-C
under sbt 2.
""",
      exampleValue {
        Server.Config.default.port
      }.assert(port => assertTrue(port == 8080)),
    ),
  )
end Reference
