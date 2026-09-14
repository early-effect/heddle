package heddle.docs

import heddle.*
import heddle.datastar.*
import heddle.sse.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.stream.ZStream
import zio.test.*

object Realtime extends DocSpecSuite:

  def doc = page("Realtime")(
    md"""
SSE, WebSocket, and Datastar are public subpackages. They are **not** on `import heddle.*`.
""",
    section("Server-Sent Events")(
      md"""
`Sse.response` wraps a `ZStream` of `ServerSentEvent`. Each event is flushed as its own HTTP
chunk. `Sse.session` is a writer you `send` / `heartbeat` into. JSON in `data` is `JsonCodec.encode`;
core does not couple SSE to JSON.
""",
      exampleZIO {
        val routes = Routes(
          Method.GET / "ticks" -> handler(
            ZIO.succeed(
              Sse.response(
                ZStream.fromIterable(
                  List(
                    ServerSentEvent(data = "0", event = Some("tick"), id = Some("0")),
                    ServerSentEvent(data = "1", event = Some("tick"), id = Some("1")),
                  )
                )
              )
            )
          )
        )
        routes(Request.get("/ticks")).flatMap(_.body.utf8).map { raw =>
          raw.contains("event: tick") && raw.contains("data: 0") && raw.contains("data: 1")
        }
      }.assert(ok => assertTrue(ok)),
      exampleDom(InteractiveRegistry.SseEvents)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/SseEvents.scala", "demo"),
    ),
    section("Datastar")(
      md"""
`heddle.datastar` is SSE with Datastar event names. `events { ... }` is a `Handler` that runs
with a `Datastar` service. `readSignals` is an extension on `Request`, not a `Request` method.

`ServerSentEventGenerator.patchElements` / `patchSignals` are the wire format Ascent (and any
Datastar client) already understands.
""",
      exampleZIO {
        val h = events {
          ServerSentEventGenerator.patchElements(
            "<div>hello</div>",
            PatchElementOptions(selector = Some("#app"), mode = ElementPatchMode.Inner),
          ) *> ServerSentEventGenerator.patchSignals("""{"count":1}""")
        }
        Routes(Method.GET / "sse" -> h).runZIO(Request.get("/sse")).flatMap(_.body.utf8).map { raw =>
          raw.contains("event: datastar-patch-elements") &&
          raw.contains("data: selector #app") &&
          raw.contains("event: datastar-patch-signals")
        }
      }.assert(ok => assertTrue(ok)),
      exampleDom(InteractiveRegistry.DatastarPatches)
        .fromSource("docs-js/src/main/scala/heddle/docs/widget/DatastarPatches.scala", "demo"),
    ),
    section("WebSocket")(
      md"""
The server speaks WebSocket upgrade (`heddle.ws`). There is no WebSocket client in this release;
HTTP/2 client and WS client are on the roadmap. Prefer SSE when a stream of events is enough.
"""
    ),
  )
end Realtime
