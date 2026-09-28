package heddle.apps.browser.counter

import heddle.*
import heddle.mcp.apps.{Grant, Shed, UiUri}
import zio.json.*

final case class Count(value: Int) derives Schema, JsonCodec

/** The counter MCP App both ends of the browser suite share: the server that serves it, and the view that renders it.
  */
object Counter:
  val show = Endpoint.get("counter").out[Count].name("show_counter").summary("Open the counter")
  val inc  = Endpoint.post("counter" / "inc").out[Count].name("inc")

  val shed = Shed(UiUri("ui://counter/view"), "Counter", Grant.launch(show))((inc = Grant.app(inc)))
