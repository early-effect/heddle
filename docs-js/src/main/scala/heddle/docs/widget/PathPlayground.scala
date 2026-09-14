package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object PathPlayground:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private val codec = """Method.GET / "users" / int("id")"""

  private def ui: URIO[Scope, UI[Any]] =
    for path <- sq("/users/1")
    yield
      val result = path.map {
        case "/users/1"   => "match  id = 1"
        case "/users/42"  => "match  id = 42"
        case "/users/ada" => "no match  int(\"id\") rejects non-digits → 404"
        case "/users"     => "no match  missing capture"
        case other        => s"no match  $other"
      }
      E.div(
        DocsUi.Lab,
        E.div(DocsUi.Hint, "Codec: ", E.code(codec)),
        E.div(
          DocsUi.Row,
          DocsUi.modeButton(path, "/users/1"),
          DocsUi.modeButton(path, "/users/42"),
          DocsUi.modeButton(path, "/users/ada"),
          DocsUi.modeButton(path, "/users"),
        ),
        E.pre(DocsUi.Mono, result),
      )
  // specular:end
end PathPlayground
