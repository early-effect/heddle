package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object SseEvents:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for n <- sq(0)
    yield
      val frame = n.map { i =>
        s"""event: tick
id: $i
data: $i

# Sse.response flushes each ServerSentEvent as its own chunk.
# Sse.session { w => w.send(...) *> w.heartbeat } is the writer form."""
      }
      E.div(
        DocsUi.Lab,
        E.div(
          DocsUi.Row,
          E.button(DocsUi.Btn, Events.onClick(_ => n.update(_ + 1)), "next event"),
          E.span(DocsUi.Hint, n.map(i => s"id $i")),
        ),
        E.pre(DocsUi.Mono, frame),
      )
  // specular:end
end SseEvents
