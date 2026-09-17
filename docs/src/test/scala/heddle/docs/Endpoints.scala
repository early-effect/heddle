package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object Endpoints extends DocSpecSuite:

  def doc = page("Endpoints and OpenAPI")(
    md"""
`Endpoint` is the typed description of one operation. `Api.bind` attaches the function. OpenAPI
3.1 and Swagger UI are projections. The teaching path is [Operations are an AST](operations-are-an-ast.html)
then [Docs and HTTP fall out](docs-and-http-fall-out.html).
""",
    illustrationIO(Hub.Lives.openApi).live.withMountKey(InteractiveRegistry.EndpointsOpenApi),
    section("The DSL")(
      md"""
`.inJson` / `.out` / `.outError` need `Schema` and `JsonCodec`. `.query`, `.header`, `.auth`,
`.summary`, `.tag` are documentation that also drives decoding.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.get("/shows/1")).map(res => (res.status, res.body.asString))
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body.contains("Evening bill"))
      },
    ),
    section("OpenAPI is a projection")(
      exampleZIO {
        BoxOffice.seed.map { store =>
          val json = BoxOffice.api(store).openApi.toJson
          json.contains("/shows/{id}") && json.contains("One bill")
        }
      }.assert(ok => assertTrue(ok)),
      md"""
The live projection is on [Docs and HTTP fall out](docs-and-http-fall-out.html).
""",
    ),
    section("Errors")(
      md"""
`.outError[E](Status.NotFound)` maps a typed `E` to a status. `OpArgs` is how MCP flattens path,
query, and JSON body into tool arguments; non-JSON bodies are not promotable.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.get("/shows/99")).map(_.status)
        }
      }.assert(status => assertTrue(status == Status.NotFound)),
    ),
  )
end Endpoints
