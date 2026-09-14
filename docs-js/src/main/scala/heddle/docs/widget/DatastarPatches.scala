package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object DatastarPatches:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for kind <- sq("elements")
    yield
      val frame = kind.map {
        case "signals" =>
          """event: datastar-patch-signals
data: signals {"count":1}

ServerSentEventGenerator.patchSignals(json)
readSignals[A] is a Request extension, not on import heddle.*."""
        case _ =>
          """event: datastar-patch-elements
data: selector #app
data: mode inner
data: elements <div>hello</div>

ServerSentEventGenerator.patchElements(html, PatchElementOptions(...))"""
      }
      E.div(
        DocsUi.Lab,
        E.div(DocsUi.Row, DocsUi.modeButton(kind, "elements"), DocsUi.modeButton(kind, "signals")),
        E.pre(DocsUi.Mono, frame),
      )
  // specular:end
end DatastarPatches
