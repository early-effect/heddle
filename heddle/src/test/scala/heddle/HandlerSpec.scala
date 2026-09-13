package heddle

import zio.*
import zio.test.*

object HandlerSpec extends ZIOSpecDefault:
  def spec =
    suite("Handler")(
      test("mapError changes the error type"):
        val h = Handler.fromZIO(ZIO.fail("boom")).mapError(_.length)
        h(Request.get("/")).either.map { err =>
          assertTrue(err == Left(4))
        }
      ,
      test("catchAll turns an error into a response"):
        val h = Handler.fromZIO(ZIO.fail("boom")).catchAll(msg => ZIO.succeed(Response.badRequest(msg)))
        h(Request.get("/")).map { res =>
          assertTrue(res.status == Status.BadRequest, res.body.asString == "boom")
        }
      ,
      test("orElse uses the fallback when the left handler fails"):
        val h =
          Handler.fromZIO(ZIO.fail("boom")).orElse(Handler.text("fallback"))
        h(Request.get("/")).map { res =>
          assertTrue(res.body.asString == "fallback")
        }
      ,
      test("handler constructors wrap Response, ZIO, and Request => ZIO"):
        val a = handler(Response.text("a"))
        val b = handler(ZIO.succeed(Response.text("b")))
        val c = handler((_: Request) => ZIO.succeed(Response.text("c")))
        for
          ra <- a(Request.get("/"))
          rb <- b(Request.get("/"))
          rc <- c(Request.get("/"))
        yield assertTrue(ra.body.asString == "a", rb.body.asString == "b", rc.body.asString == "c"),
    ) @@ TestAspect.timeout(5.seconds)
end HandlerSpec
