package heddle.docs

import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object WebApp extends DocSpecSuite:

  def doc = page("A web app is another client")(
    md"""
A browser is not a fourth implementation. It is an HTTP client of `api.routes`.

The directory on [the landing](index.html) is that client: list people, open one. Same
`BoundOp` Swagger and Grok call. JSON is a detail, not the UI.
""",
    illustrationIO(Hub.Lives.desk).live.withMountKey(InteractiveRegistry.DeskApp),
    section("Clone if you want the process")(
      md"""
```bash
sbt example/run
```

That serves `/preview`, `/docs`, and `POST /mcp` from the real `Api`. The widget above is the
same directory, replayed.
"""
    ),
    section("Writes still go through the Api")(
      md"""
`POST /users` is bound, documented, and (in the example) OAuth-gated. The preview UI will not
invent a second create path. If you add a host later, you bind it to this `Api` or you are
doing it wrong.
"""
    ),
  )
end WebApp
