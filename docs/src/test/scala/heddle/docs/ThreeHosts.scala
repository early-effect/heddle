package heddle.docs

import ascent.*
import ascent.dsl.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object ThreeHosts extends DocSpecSuite:

  def doc = page("One capability, many hosts")(
    illustration {
      Hub.mount(
        E.p(
          Hub.Lead,
          "The directory API is a handful of ",
          E.code("Endpoint"),
          "s bound once.",
        ),
        E.p(
          Hub.Copy,
          "HTTP, OpenAPI, MCP Streamable HTTP, and stdio are hosts of that bind. They are not three implementations. CLI is the same ",
          E.code("OpArgs"),
          " shape, not shipped yet.",
        ),
        Hub.poster,
      )
    }.assert(_ => assertTrue(true)),
    section("Write the capability once")(
      md"""
`getUser` is an `Endpoint`. `.mcp` promotes it as a tool. `Api.bind` is the function agents and
HTTP both run.
"""
    ),
    section("HTTP")(
      md"""
`api.routes` is `Routes`. A GET is an ordinary `Request`. Humans hit this from a browser or
Swagger. Systems hit it from `Client`.
"""
    ),
    section("MCP Streamable HTTP")(
      md"""
`Mcp.from(api)` speaks JSON-RPC on `POST /mcp`. `tools/list` is the promoted set.
`tools/call` with `get_users_id` runs the same function as `GET /users/1`.
"""
    ),
    section("stdio")(
      md"""
`mcp.stdio()` is the same engine on a pipe: one JSON-RPC line in, one line out. Local agent
runtimes spawn `sbt "example/run -- --mcp-stdio"` (or a published main) and speak that framing.
"""
    ),
    section("Toggle the host")(
      md"""
The same get-user operation as HTTP, OpenAPI, `tools/call`, and a stdio line. Payloads match the
examples above.
""",
      illustrationIO(Hub.Lives.hostFanout).live.withMountKey(InteractiveRegistry.HostFanout),
    ),
  )
end ThreeHosts
