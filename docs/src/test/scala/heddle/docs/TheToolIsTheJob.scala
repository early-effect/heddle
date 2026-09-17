package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.mcp.protocol.JsonRpc.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.EncoderOps
import zio.test.*

object TheToolIsTheJob extends DocSpecSuite:

  def doc = page("The tool is the job")(
    md"""
Agents call tools to finish jobs. HTTP still needs resource routes. Those are different grains of the same functions, not two APIs.

Write the ZIO functions. Bind fine-grained `Endpoint`s for HTTP and OpenAPI. Do not mark them `.mcp`. Bind one coarser `Endpoint` whose handler composes those functions. Mark that one with `Api.job`. `Mcp.from` lists the job. HTTP still serves the ingredients.
""",
    section("Two grains")(
      md"""
```scala
api
  .resource(createHold)(inventory.hold)
  .job(seatTheParty) { party =>
    for
      show          <- shows.get(party.showId)
      _             <- ZIO.fail(SeatingError.SoldOut(party.showId)).when(show.remaining < party.size)
      seats         <- inventory.holdBlock(party.showId, party.size)
      (total, code) <- pricing.quote(show, seats).zipPar(pickup.mint)
      order         <- orders.record(party, seats, total)
    yield PartySeated(order.id, seats, total, code)
  }
```

`resource` is `bind` without promotion. `job` is `bind` as a promoted tool. `party` is typed from the `Endpoint`. Composition is ordinary ZIO.
"""
    ),
    section("What agents see")(
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
            mcp.handle(BoxOffice.rpc("tools/list", obj())).map { out =>
              val json = out.get.toJson
              json.contains("seat_the_party") &&
              json.contains("get_show") &&
              json.contains("list_shows") &&
              !json.contains("create_hold")
            }
          }
        }
      }.assert(listed => assertTrue(listed)),
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.post("/parties", Body.json("""{"showId":1,"size":2}"""))).map { res =>
            res.status == Status.Created && res.body.asString.contains("P1001")
          }
        }
      }.assert(ok => assertTrue(ok)),
    ),
    md"""
An agent should not search for "create hold", then "create order", then hope a pickup code exists. It should call `seat_the_party` and get a `PartySeated`. The resource routes are for browsers, Swagger, and any client that wants the ingredients.
""",
  )
end TheToolIsTheJob
