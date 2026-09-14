package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object ThreeHostsExplorer:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for host <- sq("HTTP")
    yield
      val body = host.map {
        case "MCP" =>
          """POST /mcp
Content-Type: application/json

{"jsonrpc":"2.0","id":1,"method":"tools/call",
 "params":{"name":"get_users_id","arguments":{"id":1}}}

→ {"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"{\"id\":1,\"name\":\"Ada\"}"}]}}"""
        case "stdio" =>
          """stdin  → {"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_users_id","arguments":{"id":1}}}
stdout ← {"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"{\"id\":1,\"name\":\"Ada\"}"}]}}

one JSON-RPC line in, one line out. mcp.stdio() is that loop."""
        case _ =>
          """GET /users/1 HTTP/1.1

HTTP/1.1 200 OK
content-type: application/json

{"id":1,"name":"Ada"}"""
      }
      E.div(
        DocsUi.Lab,
        E.div(
          DocsUi.Row,
          DocsUi.modeButton(host, "HTTP"),
          DocsUi.modeButton(host, "MCP"),
          DocsUi.modeButton(host, "stdio"),
        ),
        E.pre(DocsUi.Mono, body),
        E.div(DocsUi.Hint, "Same BoundOp: get_users_id. HTTP, MCP, and stdio are hosts, not copies."),
      )
  // specular:end
end ThreeHostsExplorer
