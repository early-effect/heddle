package heddle.docs

import ascent.*
import ascent.dsl.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object Operations extends DocSpecSuite:

  def doc = page("Operations are an AST")(
    illustration {
      Hub.mount(
        E.p(
          Hub.Lead,
          "An ",
          E.code("Endpoint"),
          " is the typed description of one operation.",
        ),
        E.p(
          Hub.Copy,
          "Method, path, inputs, outputs, errors, security, summary, tags, MCP promotion, hints. It is data. ",
          E.code("Api.bind"),
          " attaches the function later. OpenAPI and MCP read ",
          E.code("EndpointDoc"),
          ". HTTP runs ",
          E.code("decodeIn"),
          " / ",
          E.code("encodeOut"),
          ".",
        ),
      )
    }.assert(_ => assertTrue(true)),
    section("Write getShow")(
      md"""
```scala
val getShow =
  Endpoint
    .get("shows" / int("id"))
    .out[Show]
    .outError[NotFound](Status.NotFound)
    .name("get_show")
    .summary("One bill")
    .hints(Hint.ReadOnly)
```

`Api.job` is explicit promotion. Heddle will not auto-export every REST operation as a tool.
`.inJson` / `.out` need `Schema` and `JsonCodec` (`derives Schema, JsonCodec` on the domain type).
"""
    ),
    section("Click a field")(
      md"""
Each field on `EndpointDoc` is read by a different interpreter. Click through them.
""",
      illustrationIO(Hub.Lives.opAnatomy).live.withMountKey(InteractiveRegistry.OpAnatomy),
    ),
    section("Next")(
      md"""
[Bind the effect](bind-the-effect.html) is the only implementation.
"""
    ),
  )
end Operations
