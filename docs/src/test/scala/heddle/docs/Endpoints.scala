package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Endpoints extends DocSpecSuite:

  def doc = page("Endpoints and OpenAPI")(
    md"""
`Endpoint` is the typed description of one operation: method, path, inputs, outputs, errors,
security. `Api.bind` attaches the function. OpenAPI 3.1 and Swagger UI are projections of that
description. User JSON still goes through `JsonCodec`. Core uses zio-json only for library-owned
documents (the OpenAPI spec).
""",
    section("Endpoint DSL")(
      md"""
`.inJson` / `.out` / `.outError` need `Schema` and `JsonCodec`. `.query`, `.header`, `.auth`,
`.summary`, `.tag` are documentation that also drives decoding. `import heddle.json.given` is
enough for any type with zio-json instances. Other libraries (circe, jsoniter) satisfy the same
type class with a `given JsonCodec`.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          val users = Users.api(store, nextId)
          users
            .routes(Request.post("/users", Body.json("""{"name":"Bob"}""")))
            .map(res => (res.status, res.body.asString))
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Created, body.contains("Bob"))
      },
    ),
    section("OpenAPI is a projection")(
      md"""
`Api.openApi` / `users.openApi` / `OpenApi.from(...)` render the spec. `spec.routes("docs")`
serves Swagger at `/docs` and `/docs/openapi.json`. Content keys are charset-less
(`application/json`). `Endpoint.auth(SecurityScheme.HttpBearer())` documents `securitySchemes` so
Swagger gets Authorize.
""",
      exampleZIO {
        Users.seed.map { (store, nextId) =>
          val json = Users.api(store, nextId).openApi.toJson
          (
            json.contains("/users/{id}"),
            json.contains("\"get\""),
            json.contains("Get a user"),
          )
        }
      }.assert { case (path, get, summary) =>
        assertTrue(path, get, summary)
      },
      exampleDom(InteractiveRegistry.OpenApiPreview)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/OpenApiPreview.scala", "demo"),
    ),
    section("Errors")(
      md"""
`.outError[E](Status.NotFound)` maps a typed `E` to a status. Missing credentials from `Auth.*`
are 401 with `WWW-Authenticate`. `OpArgs` is how MCP flattens path, query, and JSON body into
tool arguments; non-JSON bodies are not promotable.
""",
      exampleZIO {
        Users.seed.flatMap { (store, nextId) =>
          Users.api(store, nextId).routes(Request.get("/users/99")).map(_.status)
        }
      }.assert(status => assertTrue(status == Status.NotFound)),
    ),
  )
end Endpoints
