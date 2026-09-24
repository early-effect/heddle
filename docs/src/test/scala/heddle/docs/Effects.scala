package heddle.docs

import heddle.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Effects extends DocSpecSuite:

  def doc = page("Effects")(
    md"""
The AST is the contract. `ZIO[R, E, A]` is the body.

- `R` is the environment. The example's writes need `JwtClaim`. Reads need `Any`.
- `E` is the domain error, ideally a sealed trait or enum. `.outErrors[SeatingError](...)` gives
  each case its own HTTP status. MCP sees `isError`. Do not leak SQL or JWT library types into `E`.
- `A` is the success value `Schema` already described.

`Handler` is `Request => ZIO[R, E, Response]` when you are still on raw routes. `Api.bind` is
`In => ZIO[R, E, Out]` once the endpoint exists. Both are ordinary ZIO. There is no event-loop
rule. Accept, read, and write run on the JVM, on Node, and on Native.
""",
    illustrationIO(Hub.Lives.effectTrace).live.withMountKey(InteractiveRegistry.EffectWalk),
    section("Walk a request")(
      md"""
Click a step. Decode is not your code. The bind is. Encode is not your code. Interrupting the
connection interrupts the fiber. Same widget as [Bind the effect](bind-the-effect.html).
"""
    ),
    section("Middleware is still an effect")(
      md"""
`@@` applies middleware. `a ++ b` composes: the request hits `b` first (outer), then `a`.
`requestId`, `cors`, `compress`, `debug`, and the auth helpers live here. They are `Routes => Routes`,
not a parallel effect system.
""",
      exampleZIO {
        val routes =
          Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestId()
        routes(Request.get("/x")).map { res =>
          res.header("X-Request-Id").exists(_.nonEmpty)
        }
      }.assert(hasId => assertTrue(hasId)),
    ),
    section("Streams are streams")(
      md"""
Bodies on the wire are streams. `Body.asString` / `asBytes` are for in-memory bodies.
SSE is a `ZStream` of `ServerSentEvent`. Do not pretend a stream is a `List`. Interrupt the
client, the stream fiber stops.
"""
    ),
  )
end Effects
