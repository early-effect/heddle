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
Api("Box office", "0.1.0")
  .job(getShow) { id =>
    shows.get(id)
  }
```

The function is `In => ZIO[R, E, Out]`. Here `In` is `Int`, `E` is `NotFound`, `Out` is `Show`.
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
[The tool is the job](the-tool-is-the-job.html). Compose that ZIO. Promote the bind agents should call.
"""
    ),
  )
end BindEffect
