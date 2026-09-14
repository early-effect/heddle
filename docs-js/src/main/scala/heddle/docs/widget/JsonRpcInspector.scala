package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object JsonRpcInspector:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for side <- sq("request")
    yield
      val body = side.map {
        case "response" =>
          """{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [
      { "type": "text", "text": "{\"id\":1,\"name\":\"Ada\"}" }
    ]
  }
}"""
        case _ =>
          """{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "get_users_id",
    "arguments": { "id": 1 },
    "_meta": { "protocolVersion": "2026-07-28" }
  }
}"""
      }
      E.div(
        DocsUi.Lab,
        E.div(DocsUi.Row, DocsUi.modeButton(side, "request"), DocsUi.modeButton(side, "response")),
        E.pre(DocsUi.Mono, body),
        E.div(DocsUi.Hint, "Same function as GET /users/1. Protocol revision 2026-07-28 only."),
      )
  // specular:end
end JsonRpcInspector
