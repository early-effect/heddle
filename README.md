# Heddle

HTTP as an effect. Loom under the floor.

A ZIO-first HTTP library: handlers are `Request => ZIO[R, E, Response]`, routing is data, middleware is `@@`. Write a capability once (`BoundOp` / `Api`) and host it for humans, systems, and agents: HTTP + OpenAPI / Swagger, MCP 2026-07-28 over Streamable HTTP, or a stdio process.

Docs (tests-as-docs, live examples): [https://www.earlyeffect.rocks/heddle/](https://www.earlyeffect.rocks/heddle/)

`import heddle.*` is a curated facade over `heddle.http`, `heddle.route`, `heddle.endpoint`, `heddle.server`, `heddle.client`, and `heddle.error`. SSE, WebSocket, and Datastar stay in their own packages.

## Install

Core (ZIO only):

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "0.2.0"
```

Optional artifacts, same version: `heddle-zio-json`, `heddle-mcp`, `heddle-oauth`, `heddle-brotli`.

## Routes

```scala
import heddle.*
import zio.*

val routes = Routes(
  Method.GET / "health"            -> Handler.text("ok"),
  Method.GET / "users" / int("id") -> { (id: Int) =>
    ZIO.succeed(Response.text(id.toString))
  },
)

val app = routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug)

object Hello extends HeddleApp:
  def routes = app
```

Promote selected endpoints with `.mcp`, then `Mcp.from(api)`. Agents hit `POST /mcp` or a `--mcp-stdio` process.

## Run the example

```bash
sbt example/run                      # users API + embedded OP + Swagger at /docs, MCP at /mcp
sbt "example/run -- --mcp-stdio"     # same Api on stdio
```

## Modules

| Artifact | Depends on | Role |
| --- | --- | --- |
| `heddle` | ZIO | HTTP types, routes, middleware, Loom server, Schema, Endpoint, OpenAPI |
| `heddle-zio-json` | heddle, zio-json | `given JsonCodec[A]` from zio-json |
| `heddle-mcp` | heddle, heddle-zio-json | MCP 2026-07-28 over `Api` / `BoundOp` |
| `heddle-oauth` | heddle, heddle-zio-json | JOSE, resource server, OAuth client, OIDC provider |
| `heddle-brotli` | heddle | RFC 7932 `br` encoder / decoder (no JNI) |

JDK 21+. Full guide: [earlyeffect.rocks/heddle](https://www.earlyeffect.rocks/heddle/).
