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
Model an endpoint's errors as a sealed hierarchy: an `enum`, or a `sealed trait` with case classes
and case objects. `derives Schema, JsonCodec` on the type is all it needs. `.outErrors[E]` pins the
type, then takes one `ErrorCase` per case:

```scala
enum SeatingError derives Schema, JsonCodec:
  case SoldOut(showId: Int)
  case NoBlock(showId: Int, size: Int)

Endpoint
  .post("parties")
  .inJson[Party]
  .out[PartySeated](Status.Created)
  .outErrors[SeatingError](
    ErrorCase[SeatingError.SoldOut](Status.Conflict),
    ErrorCase[SeatingError.NoBlock](Status.UnprocessableContent),
  )
```

The list is checked against the type at compile time. A missing case, a case listed twice, or a
type that is not a case of `E` does not compile, and the error names the case. Cases may share a
status. A nested `sealed trait` counts as one case covering all of its leaves.

The body is `JsonCodec[E]`'s encoding: `{"SoldOut":{"showId":1}}` for a case with fields,
`{"Closed":{}}` for a singleton next to cases with fields, and a bare `"Red"` when every case is a
singleton. OpenAPI documents each status with that case's schema. `Client.call` reads the status
back to the typed case, and MCP returns the same body as `isError`. Derived `Schema` follows
zio-json's default sum encoding; `@jsonDiscriminator`, `@jsonHint`, or a custom
`JsonCodecConfiguration` change the wire shape without changing the `Schema`.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          val routes              = BoxOffice.api(store).routes
          def seat(party: String) = routes(Request.post("/parties", Body.json(party)))
          for
            soldOut <- seat("""{"showId":1,"size":13}""")
            noBlock <- seat("""{"showId":3,"size":2}""")
          yield (soldOut.status, soldOut.body.asString, noBlock.status, noBlock.body.asString)
        }
      }.assert { case (soldOut, soldOutBody, noBlock, noBlockBody) =>
        assertTrue(
          soldOut == Status.Conflict,
          soldOutBody == """{"SoldOut":{"showId":1}}""",
          noBlock == Status.UnprocessableContent,
          noBlockBody == """{"NoBlock":{"showId":3,"size":2}}""",
        )
      },
      md"""
For a single error type that needs no per-case status, `.outError[E](Status.NotFound)` sends every
error with one status. `OpArgs` is how MCP flattens path, query, and JSON body into tool
arguments; non-JSON bodies are not promotable.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.get("/shows/99")).map(_.status)
        }
      }.assert(status => assertTrue(status == Status.NotFound)),
    ),
  )
end Endpoints
