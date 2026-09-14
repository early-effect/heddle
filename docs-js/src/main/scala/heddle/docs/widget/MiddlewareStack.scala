package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object MiddlewareStack:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for order <- sq("id-cors")
    yield
      val view = order.map {
        case "cors-id" =>
          """routes @@ (Middleware.cors() ++ Middleware.requestId())

request → requestId → cors → handler
`a ++ b` applies a first, then wraps with b. Incoming hits b, then a."""
        case _ =>
          """routes @@ (Middleware.requestId() ++ Middleware.cors())

request → cors → requestId → handler
cors is outer. requestId still stamps X-Request-Id before the handler."""
      }
      E.div(
        DocsUi.Lab,
        E.div(
          DocsUi.Row,
          DocsUi.modeButton(order, "id-cors"),
          DocsUi.modeButton(order, "cors-id"),
        ),
        E.pre(DocsUi.Mono, view),
      )
  // specular:end
end MiddlewareStack
