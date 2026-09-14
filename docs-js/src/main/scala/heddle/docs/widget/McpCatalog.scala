package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object McpCatalog:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for mode <- sq("promoted")
    yield
      val tools = mode.map {
        case "catalog" =>
          """tools/list  (withCatalog)

- get_users_id     GET /users/{id}   hint: ReadOnly
- get_users        GET /users        hint: ReadOnly
- search_operations
- invoke           # post_users lives here, not as a promoted tool"""
        case _ =>
          """tools/list  (promoted only)

- get_users_id     GET /users/{id}   hint: ReadOnly
- get_users        GET /users        hint: ReadOnly

createUser has no .mcp, so it is absent until withCatalog."""
      }
      E.div(
        DocsUi.Lab,
        E.div(DocsUi.Row, DocsUi.modeButton(mode, "promoted"), DocsUi.modeButton(mode, "catalog")),
        E.pre(DocsUi.Mono, tools),
      )
  // specular:end
end McpCatalog
