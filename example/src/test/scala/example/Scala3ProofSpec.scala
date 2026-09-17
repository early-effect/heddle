package example

import heddle.*
import zio.*
import zio.test.*

/** Prove Scala 3 / ZIO shapes used by the box office before writing the bind that way. */
object Scala3ProofSpec extends ZIOSpecDefault:
  enum Boom derives Schema, JsonCodec:
    case Out(id: Int)

  def spec =
    suite("box office Scala 3 proofs")(
      test("ZIO.fail.when fails only when the predicate is true"):
        for
          skipped <- ZIO.fail("x").when(false).either
          hit     <- ZIO.fail("x").when(true).either
        yield assertTrue(skipped.isRight, hit == Left("x"))
      ,
      test("Ref.modify Option someOrFail is the miss"):
        for
          r    <- Ref.make(Map(1 -> "a"))
          hit  <- r.modify(m => (m.get(1), m)).someOrFail("miss")
          miss <- r.modify(m => (m.get(2), m)).someOrFail("miss").either
        yield assertTrue(hit == "a", miss == Left("miss"))
      ,
      test("zipPar unpacks in a for-comprehension"):
        for (a, b) <- ZIO.succeed(1).zipPar(ZIO.succeed("x"))
        yield assertTrue(a == 1, b == "x")
      ,
      test("derives JsonCodec is enough for Endpoint.outError"):
        val ep     = Endpoint.get("x").out[Show].outError[Boom](Status.Conflict)
        val routes = ep.implement(_ => ZIO.fail(Boom.Out(1)))
        routes(Request.get("/x")).map { res =>
          assertTrue(res.status == Status.Conflict, res.body.asString.contains("Out"))
        }
      ,
      test("job eta-expands a method without ascription"):
        val ep = Endpoint
          .get("shows" / int("id"))
          .out[Show]
          .outError[NotFound](Status.NotFound)
          .name("get_show")
        def get(id: Int): IO[NotFound, Show] =
          ZIO.fail(NotFound(s"nope $id"))
        val api = Api("t", "1").job(ep)(get)
        assertTrue(api.openApi.endpoints.head.promoted, api.openApi.endpoints.head.toolName == "get_show"),
    ) @@ TestAspect.timeout(5.seconds)
end Scala3ProofSpec
