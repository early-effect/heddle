package heddle.docs

import ascent.*
import ascent.ast.Attr
import ascent.domtypes.AttrValue
import ascent.dsl.*
import heddle.docs.ui.Hub
import specular.MountPoint

/** Full-document Ascent landing. `BuildSite.afterBuild` SSRs this over Specular's generated index. */
object Landing:

  def document: UI[Any] =
    E.div(
      Hub.Screen,
      header,
      hero,
      mount(InteractiveRegistry.LandingPoster, Hub.posterAt("HTTP", display = true, live = None)),
      mount(InteractiveRegistry.DeskApp, Hub.desk(Some(1))),
      E.p(
        Hub.Hint,
        "Same bind the tests run. Replay of the AST, not a hosted server. Clone and ",
        E.code("sbt example/run"),
        " if you want the JVM process.",
      ),
      doors,
      footer,
    )

  private def header: UI[Any] =
    E.div(
      Hub.Bar,
      E.a(
        A.href("https://www.earlyeffect.rocks/"),
        E.img(Hub.Mark, A.src("images/logo.png"), A.alt("Early Effect")),
      ),
      E.a(Hub.NavLink, A.href("index.html"), E.strong("heddle")),
      E.a(Hub.NavLink, A.href("https://github.com/early-effect/heddle"), "GitHub"),
    )

  private def hero: UI[Any] =
    E.div(
      Hub.Stack,
      Hub.kicker("capability compiler"),
      E.h1(Hub.Headline, "Write the service once."),
      E.p(
        Hub.Lead,
        "HTTP, OpenAPI, and MCP are hosts of the same ",
        E.code("BoundOp"),
        ". CLI is a later interpreter. Not a second product.",
      ),
    )

  private def doors: UI[Any] =
    E.div(
      Hub.Row,
      E.a(Hub.Door, A.href("the-hub.html"), "The hub"),
      E.a(Hub.DoorGhost, A.href("install.html"), "Install"),
    )

  private def footer: UI[Any] =
    E.p(Hub.Hint, E.a(Hub.NavLink, A.href("the-hub.html"), "read the compiler"))

  /** Set by sbt-specular during `specularSite`. Tests fall back so `doc` can load. */
  private[docs] def docsVersion: String =
    Option(System.getProperty("specular.meta.version")).filter(_.nonEmpty).getOrElse("0.2.0")

  private def mount(key: String, fallback: UI[Any]): UI[Any] =
    E.div(Attr.StaticAttr(MountPoint.Attr, AttrValue.Str(key)), fallback)
end Landing
