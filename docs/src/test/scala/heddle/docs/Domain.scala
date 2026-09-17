package heddle.docs

import ascent.*
import ascent.dsl.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object Domain extends DocSpecSuite:

  def doc = page("Domain is data")(
    illustration {
      Hub.mount(
        E.p(Hub.Lead, "A hub starts as types, not as routes."),
        E.p(
          Hub.Copy,
          E.code("Schema"),
          " is what every host will read: JSON codecs, OpenAPI components, MCP tool arguments, and (later) CLI flags. ",
          E.code("JsonCodec"),
          " is how bytes move. Core never parses a user body except through ",
          E.code("JsonCodec"),
          ".",
        ),
      )
    }.assert(_ => assertTrue(true)),
    illustrationIO(Hub.Lives.schemaPoster).live.withMountKey(InteractiveRegistry.SchemaPoster),
    illustration {
      Hub.mount(
        E.h2(Hub.H2, "The directory"),
        E.p(
          Hub.Copy,
          "The running example in this repo is a people directory. Eight names. That is enough to host humans, systems, and agents.",
        ),
      )
    }.assert(_ => assertTrue(true)),
    section("The types")(
      md"""
```scala
final case class User(id: Int, name: String) derives Schema, JsonCodec
final case class NewUser(name: String) derives Schema, JsonCodec
final case class NotFound(message: String) derives Schema, JsonCodec
```
"""
    ),
    section("Why Schema exists")(
      md"""
`derives Schema` is not documentation theater. `User` becomes:

- the JSON object `{"id":1,"name":"Ada"}` on the wire
- `components.schemas.User` in OpenAPI
- the result schema of `get_users_id` for MCP
- the shape a future CLI would print and parse

Change the type. Every host moves. That is the point of a compiler.

[Operations are an AST](operations-are-an-ast.html) turns these types into `Endpoint`s.
"""
    ),
  )
end Domain
