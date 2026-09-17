package heddle.datastar

import heddle.*
import zio.*
import zio.test.*

object DatastarSpec extends ZIOSpecDefault:
  def spec =
    suite("Datastar")(
      test("events emits patch-elements and patch-signals frames"):
        val h = events {
          ServerSentEventGenerator.patchElements(
            "<div>hello</div>",
            PatchElementOptions(selector = Some("#app"), mode = ElementPatchMode.Inner),
          ) *> ServerSentEventGenerator.patchSignals("""{"count":1}""")
        }
        val routes = Routes(Method.GET / "sse" -> h)
        routes.runZIO(Request.get("/sse")).flatMap { res =>
          res.body.utf8.map { raw =>
            assertTrue(
              res.status == Status.Ok,
              res.header(HeaderName.ContentType).exists(_.contains("text/event-stream")),
              raw.contains("event: datastar-patch-elements"),
              raw.contains("data: selector #app"),
              raw.contains("data: mode inner"),
              raw.contains("data: elements <div>hello</div>"),
              raw.contains("event: datastar-patch-signals"),
              raw.contains("data: signals {\"count\":1}"),
            )
          }
        }
      ,
      test("events session provides Scope so Handler R stays Any"):
        val h: Handler[Any, Nothing] = events {
          ZIO.serviceWithZIO[Scope](_ => ServerSentEventGenerator.patchSignals("""{"n":0}"""))
        }
        h.run(Request.get("/sse")).flatMap(_.body.utf8).map { raw =>
          assertTrue(raw.contains("datastar-patch-signals"))
        }
      ,
      test("readSignals decodes a JSON body"):
        val req = Request.post("/inc", Body.json("""{"n":7}"""))
        req.readSignals[Count].map(c => assertTrue(c.n == 7)),
    ) @@ TestAspect.timeout(15.seconds) @@ TestAspect.withLiveClock

  final case class Count(n: Int) derives JsonCodec
end DatastarSpec
