# Heddle

One BoundOp. HTTP, OpenAPI, and MCP are hosts.

Docs: [https://www.earlyeffect.rocks/heddle/](https://www.earlyeffect.rocks/heddle/)

Write the operation once. Heddle serves it as HTTP, publishes OpenAPI, and exposes it as an MCP tool.

- One `BoundOp`, three hosts: HTTP, OpenAPI, and MCP
- [MCP Apps](https://www.earlyeffect.rocks/heddle/mcp-apps.html): typed `ui://` views, `_meta.ui`, and a host that clamps policy
- Streamable HTTP or stdio, including the 2025 handshake for hosts that still send `initialize`
- JVM, Scala.js, and Scala Native

## Install

Latest stable, the same coordinate as the docs:

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "0.15.2"
libraryDependencies += "rocks.earlyeffect" %% "heddle-mcp" % "0.15.2"
libraryDependencies += "rocks.earlyeffect" %% "heddle-mcp-apps" % "0.15.2"
```

Use `%%%` for Scala.js and Scala Native. `heddle`, `heddle-mcp`, `heddle-mcp-protocol`, and `heddle-mcp-apps` publish on all three. Other artifacts and platform notes are in the [reference](https://www.earlyeffect.rocks/heddle/reference.html).

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

object Hello extends HeddleApp:
  def routes = routes @@ (Middleware.requestId() ++ Middleware.cors())
```

Promote selected endpoints with `Api.job` (or `.mcp`), then `Mcp.from(api)`. The full guide is on the [docs site](https://www.earlyeffect.rocks/heddle/).

## Run the example

```bash
./histos run example --platform jvm --main example.Main
```

That serves Swagger at `/docs`, a preview UI at `/preview`, and MCP at `POST /mcp`. Details and the stdio form are on the [install page](https://www.earlyeffect.rocks/heddle/install.html).
