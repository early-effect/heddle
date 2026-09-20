package heddle.docs

import ascent.*
import ascent.dsl.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object WhyHeddle extends DocSpecSuite:

  def doc = page("The hub")(
    illustration {
      Hub.mount(
        E.p(Hub.Lead, E.strong("Heddle is a capability compiler.")),
        E.p(
          Hub.Copy,
          "You write a service idea once: domain types, operations, and an effect that runs them. That value is the AST. Everything a human, a system, or an agent uses is an interpreter of it.",
        ),
        E.ul(
          Hub.Bullets,
          E.li(E.strong("Humans"), " get HTTP, OpenAPI / Swagger, and a web UI."),
          E.li(E.strong("Systems"), " get the same HTTP, plus ", E.code("Client"), "."),
          E.li(
            E.strong("Agents"),
            " get MCP 2026-07-28 over Streamable HTTP or a stdio process, plus the 2025 handshake for hosts that still send it.",
          ),
          E.li(
            E.strong("CLI"),
            " is a later interpreter of the same ",
            E.code("OpArgs"),
            ". It is not a second product.",
          ),
        ),
        E.p(
          Hub.Copy,
          "That shape is a human-centric AI service hub. Agents do not get a shadow API. They get the same ",
          E.code("BoundOp"),
          ", marked with ",
          E.code(".mcp"),
          ".",
        ),
      )
    }.assert(_ => assertTrue(true)),
    illustrationIO(Hub.Lives.landingPoster).live.withMountKey(InteractiveRegistry.HubPoster),
    illustration {
      Hub.mount(
        E.h2(Hub.H2, "Progressive, not a conversion"),
        E.p(
          Hub.Copy,
          "Day one is ",
          E.code("Routes"),
          " and ",
          E.code("HeddleApp"),
          ". That is already a service. You can stop there.",
        ),
        E.p(
          Hub.Copy,
          E.code("Endpoint"),
          " + ",
          E.code("Api.bind"),
          " is how documentation and the other hosts appear. OpenAPI is a projection. MCP is a host protocol. stdio is the same engine on a pipe. Do not grow a second tool DSL.",
        ),
        E.h2(Hub.H2, "The effect is the body"),
        E.p(
          Hub.Copy,
          "A handler is ",
          E.code("Request => ZIO[R, E, Response]"),
          ". ",
          E.code("Api.bind"),
          " is ",
          E.code("In => ZIO[R, E, Out]"),
          ". Accept, read, and write are ordinary ZIO on the JVM (Loom by default), on Node, and on Native. Interrupt a connection and the fiber stops. Typed errors stay in ",
          E.code("E"),
          " until a host maps them to HTTP or MCP.",
        ),
        E.p(Hub.Copy, "The AST is the contract. The effect is the work. Hosts are projections."),
        E.h2(Hub.H2, "Three runtimes"),
        E.p(
          Hub.Copy,
          E.code("Server.install"),
          " is the same bind on the JVM, on Node, and on Scala Native. Loom is the JVM default scheduler, not a requirement to bind. HTTP/2 is the JVM bind. JS and Native serve HTTP/1.1. docs-js mounts Hub widgets. It is not a second ",
          E.code("Api"),
          ".",
        ),
        E.h2(Hub.H2, "Where to go"),
        E.ul(
          Hub.Bullets,
          E.li(E.a(A.href("install.html"), "Install"), " if you want a server in one file."),
          E.li(
            E.a(A.href("one-capability-many-hosts.html"), "One capability, many hosts"),
            " if you want the thesis in one demo.",
          ),
          E.li(
            E.a(A.href("index.html"), "the landing"),
            " to click Evening bill and flip HTTP / OpenAPI / MCP on the same id.",
          ),
          E.li(
            E.a(A.href("domain-is-data.html"), "Domain is data"),
            " to build a hub end to end.",
          ),
        ),
      )
    }.assert(_ => assertTrue(true)),
    section("A server is still a server")(
      md"""
This is the only snippet on this page that is meant to be copied. The rest is the hub, not a code sample.

```scala
val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
```
"""
    ),
  )
end WhyHeddle
