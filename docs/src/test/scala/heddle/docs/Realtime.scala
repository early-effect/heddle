package heddle.docs

import heddle.*
import heddle.datastar.*
import heddle.docs.ui.Hub
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
They are HTTP hosts for streams. They are not MCP tools. `OpArgs.promotable` will refuse them.
""",
    section("Server-Sent Events")(
      md"""
`Sse.response` wraps a `ZStream` of `ServerSentEvent`. Each event is flushed as its own HTTP
chunk. `Sse.session` is a writer you `send` / `heartbeat` into. JSON in `data` is `JsonCodec.encode`.
`data` may span lines. `event` and `id` are `SseField`s, which never hold a CR or LF, so a value
cannot end its field and forge another: `SseField("tick")` checks a literal at compile time,
`SseField.from(raw)` returns `Left(SseFieldError.LineBreak(at))` for a value that breaks a line,
and `SseField.of(n)` takes a number.
""",
      exampleZIO {
        val tick   = SseField("tick")
        val routes = Routes(
          Method.GET / "ticks" -> handler(
            ZIO.succeed(
              Sse.response(
                ZStream.fromIterable(
                  List(
                    ServerSentEvent(data = "0", event = Some(tick), id = Some(SseField.of(0))),
                    ServerSentEvent(data = "1", event = Some(tick), id = Some(SseField.of(1))),
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
      expectFail("""SseField("tick\ndata: forged")""").assert { errors =>
        assertTrue(errors.exists(_.message.contains("CR or LF")))
      },
      illustrationIO(Hub.Lives.sseTape).live.withMountKey(InteractiveRegistry.SseTape),
    ),
    section("Datastar")(
      md"""
`heddle.datastar` is SSE with Datastar event names. `events { ... }` is a `Handler` that runs
with a `Datastar` service. `readSignals` is an extension on `Request`, not a `Request` method.
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
      illustrationIO(Hub.Lives.datastar).live.withMountKey(InteractiveRegistry.DatastarPatches),
    ),
    section("WebSocket")(
      md"""
The server speaks WebSocket upgrade (`heddle.ws`). There is no WebSocket client in this release.
Prefer SSE when a stream of events is enough.
"""
    ),
  )
end Realtime
