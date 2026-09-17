package heddle.docs

import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object AgentsFallOut extends DocSpecSuite:

  def doc = page("Agents fall out")(
    md"""
MCP is a host protocol over `BoundOp`, not a second tool DSL. `Api.job` promotes the ops
agents should see. `Mcp.from(api)` fails on duplicate tool names or non-promotable shapes.
Native protocol is **2026-07-28**. HTTP and stdio also answer `initialize`, so Grok Build and
Cursor `url` / `command` configs work. The authoring rule is [The tool is the job](the-tool-is-the-job.html).
""",
    illustrationIO(Hub.Lives.harnessWalk).live.withMountKey(InteractiveRegistry.HarnessWalk),
    section("Promote, don't auto-export")(
      md"""
JSON in and JSON out are required (`OpArgs.promotable`). Form, bytes, and SSE stay on HTTP.

`tools/list` is promoted-only. `withCatalog` adds `search_operations` and `invoke` so agents can
still find the rest of the `Api`.
""",
      illustrationIO(Hub.Lives.catalog).live.withMountKey(InteractiveRegistry.PromoteVsCatalog),
    ),
    section("Call the same function")(
      md"""
`tools/call` with `get_show` is `GET /shows/{id}`. Click **call** on the harness above.
"""
    ),
    section("Point Grok Build at the hub")(
      md"""
With `sbt example/run` listening:

```toml
[mcp_servers.heddle-example]
url = "http://localhost:8080/mcp"
```

Same `Api` as a local process:

```toml
[mcp_servers.heddle-example]
command = "sbt"
args = ["example/run", "--", "--mcp-stdio"]
```

Ask Grok to list tools, then seat a party of 2 on the Evening bill. You should see `seat_the_party`
and a pickup code. That is the same function Swagger just called.

stdio cold-start under sbt is slow. A packaged main is what you ship. The protocol is identical.
"""
    ),
    section("Next")(
      md"""
[A web app is another client](a-web-app-is-another-client.html).
"""
    ),
  )
end AgentsFallOut
