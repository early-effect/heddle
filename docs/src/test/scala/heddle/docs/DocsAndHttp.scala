package heddle.docs

import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object DocsAndHttp extends DocSpecSuite:

  def doc = page("Docs and HTTP fall out")(
    md"""
Once the `Api` is bound, HTTP and OpenAPI are free.

```scala
val spec = users.openApi
val http = users.routes
spec.routes("docs")   // Swagger UI at /docs, spec at /docs/openapi.json
```

There is no second source of truth. Content keys are charset-less (`application/json`).
`Endpoint.auth(SecurityScheme.HttpBearer())` is what Swagger uses for Authorize.
""",
    illustration {
      Hub.swaggerWalk
    }.assert(_ => assertTrue(true)),
    section("The spec is a projection")(
      md"""
`Api.openApi` / `users.openApi` / `OpenApi.from(...)` render OpenAPI 3.1. This test reads the
JSON we serve, not a fixture we typed twice.
""",
      illustrationIO(Hub.Lives.openApi).live.withMountKey(InteractiveRegistry.OpenApiPreview),
    ),
    section("Try it on the live hub")(
      md"""
```bash
sbt example/run
```

Open [http://localhost:8080/docs](http://localhost:8080/docs).

1. `GET /users/1` → Ada.
2. Authorize against the embedded OP (seed `ada` / `ada`).
3. `POST /users` with `{"name":"Grace"}`.

Swagger is a host of the same `Api` as `GET /users/1` in these tests. The stylized Try-it-out
above is the map. The process is the territory.
""",
      illustrationIO(Hub.Lives.swaggerWalk).live.withMountKey(InteractiveRegistry.SwaggerWalk),
    ),
    section("Next")(
      md"""
[Agents fall out](agents-fall-out.html). `.mcp` is the only extra mark.
"""
    ),
  )
end DocsAndHttp
