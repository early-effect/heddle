package heddle.docs

import heddle.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object WhyHeddle extends DocSpecSuite:

  def doc = page("Why Heddle")(
    md"""
Heddle is **HTTP as an effect**. Loom sits under the floor.

A handler is `Request => ZIO[R, E, Response]`. Routing is data. Middleware is `@@`. You write a
capability once (`BoundOp` / `Api`) and host it for the readers that actually show up:

- **Humans** get HTTP plus OpenAPI / Swagger.
- **Systems** get the same HTTP, plus `Client`.
- **Agents** get MCP 2026-07-28 over Streamable HTTP (`POST /mcp`) or a stdio process.

That is the product. The rest of this site is how to grow into it without a rewrite.

```mermaid
flowchart LR
  Op[BoundOp / Api]
  Op --> HTTP[HTTP + OpenAPI]
  Op --> McpHttp[MCP Streamable HTTP]
  Op --> Stdio[MCP stdio]
  HTTP --> Humans[Humans and systems]
  McpHttp --> Agents[Agents]
  Stdio --> Local[Local agent processes]
```
""",
    section("Progressive, not a conversion")(
      md"""
Day one is `Routes` and `HeddleApp`. That is already a service. You can stop there.

`Endpoint` + `Api.bind` is how documentation and the other hosts appear: OpenAPI is a projection,
MCP is a host protocol, stdio is the same engine on a pipe. Do not grow a second tool DSL.
`BoundOp` is the capability.
""",
      exampleZIO {
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request.get("/health")).map(res => (res.status, res.body.asString))
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body == "ok")
      },
    ),
    section("Effect safety is the default")(
      md"""
Accept, read, and write are ordinary ZIO on Loom (`Runtime.enableLoomBasedExecutor`). There is no
event-loop rule and no thread-local contract you have to remember. Interrupt a connection and the
fiber stops; open a body stream and it closes with the request.

JDK 21+ is required. Typed errors stay in `E` until a handler maps them to a `Response`. `Task` is
an edge, not the public shape of a route.
"""
    ),
    section("Performance")(
      md"""
Throughput is on-par with zio-http, without sacrificing effect safety. We measure that with
`./scripts/bench-compare.sh` in the repo; the number that matters is the one on your hardware, not
a leaderboard in these docs.
"""
    ),
  )
end WhyHeddle
