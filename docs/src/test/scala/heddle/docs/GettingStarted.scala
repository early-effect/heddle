package heddle.docs

import heddle.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object GettingStarted extends DocSpecSuite:

  def doc = page("Install")(
    md"""
One bind on the JVM, on Node, and on Scala Native. MCP, OAuth, and brotli are optional
artifacts on the same version. JSON is zio-json on core. JVM is JDK 21+. `heddle` and
`heddle-mcp` publish for all three runtimes (`%%%`). `heddle-oauth` is JVM and JS.
`HeddleApp` is JVM-only.

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "${Landing.docsVersion}"
libraryDependencies += "rocks.earlyeffect" %% "heddle-mcp" % "${Landing.docsVersion}"
```

`import heddle.*` is the facade. SSE, WebSocket, and Datastar stay in their own packages.
""",
    section("A server in one file")(
      md"""
This is already a service. You can stop here. The hub appears when you promote selected routes
to `Endpoint` / `Api`. That is the next page, not a rewrite of this one.

```scala
val routes = Routes(
  Method.GET / "health"            -> Handler.text("ok"),
  Method.GET / "users" / int("id") -> { (id: Int) =>
    ZIO.succeed(Response.text(id.toString))
  },
)
val app = routes @@ (Middleware.requestId() ++ Middleware.cors())
```
""",
      md"""
To bind a port:

```scala
object Hello extends HeddleApp:
  def routes = app
```

`HeddleApp` serves `routes` and exits 0 on Ctrl-C under sbt 2 (JVM).
`Server.serve(app).provide(Server.Config.defaults)` is the same bind without the trait, and
that is what JS and Native use.
""",
    ),
    section("Run the example hub")(
      md"""
This repo's example is one process with HTTP, Swagger, MCP, an embedded OpenID provider, and a
directory UI:

```bash
sbt example/run                      # http://localhost:8080/docs  /preview  POST /mcp
sbt "example/run -- --mcp-stdio"     # same Api, stdio JSON-RPC
```

Seed user `ada` / `ada`. Machine client `machine` / `secret`.

Then walk [Domain is data](domain-is-data.html) to see how that hub is assembled.
"""
    ),
  )
end GettingStarted
