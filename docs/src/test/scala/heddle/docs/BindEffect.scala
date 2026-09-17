package heddle.docs

import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object BindEffect extends DocSpecSuite:

  def doc = page("Bind the effect")(
    md"""
`Api.bind` is the implementation. The AST does not run itself.

```scala
Api("Users", "0.1.0")
  .bind(getUser) { id =>
    store.get.map(_.get(id).toRight(NotFound(s"user $$id"))).flatMap(ZIO.fromEither)
  }
```

The function is `In => ZIO[R, E, Out]`. Here `In` is `Int`, `E` is `NotFound`, `Out` is `User`.
HTTP will decode the path, run this ZIO, and encode the result. MCP will flatten arguments with
`OpArgs`, run the same ZIO, and wrap the JSON as tool content.
""",
    section("The trace")(
      md"""
Step through a GET. Decode is not your code. The bind is. Encode is not your code. Interrupting
the connection interrupts the fiber.
""",
      illustrationIO(Hub.Lives.effectTrace).live.withMountKey(InteractiveRegistry.EffectTrace),
    ),
    section("Next")(
      md"""
[Docs and HTTP fall out](docs-and-http-fall-out.html). You do not write OpenAPI by hand.
"""
    ),
  )
end BindEffect
