package heddle.docs.ui

import ascent.*
import ascent.dsl.*
import zio.*

/** Visual language for Heddle docs. Sits on Early Effect chalkboard tokens (`--specular-*`) so light and dark both read
  * as instruments on the board, not a second brand.
  */
object Hub:

  val ink     = Color.keyword("var(--specular-text)")
  val muted   = Color.keyword("var(--specular-muted)")
  val line    = Color.keyword("var(--specular-border)")
  val panel   = Color.keyword("var(--specular-surface)")
  val board   = Color.keyword("var(--specular-bg)")
  val code    = Color.keyword("var(--specular-code-bg)")
  val codeInk = Color.keyword("var(--specular-code-fg)")
  val accent  = Color.keyword("var(--specular-accent)")
  val link    = Color.keyword("var(--specular-link)")
  val radius  = "var(--specular-radius)"
  val font    = "var(--specular-font-sans)"
  val chalkOk = Color.hex("#7d9b6a")
  val ring    = Color.hex("#c46a52")

  final case class Person(id: Int, name: String, role: String)

  val people: List[Person] = List(
    Person(1, "Ada", "first programmer"),
    Person(2, "Grace", "COBOL, compilers"),
    Person(3, "Alan", "computation"),
    Person(4, "Margaret", "Apollo software"),
    Person(5, "Edsger", "structured programming"),
    Person(6, "Barbara", "abstraction"),
    Person(7, "Donald", "The Art of Computer Programming"),
    Person(8, "John", "FORTRAN"),
  )

  object Screen
      extends CssClass(
        S.display.grid,
        S.gap(1.5.rem),
        S.maxWidth.px(920),
        Declaration("margin", "0 auto"),
        S.padding(1.5.rem, 1.25.rem),
        S.color(ink),
        Declaration("font-family", font),
        Declaration("min-height", "100vh"),
      )

  object Headline
      extends CssClass(
        S.fontSize.px(40),
        S.fontWeight(650),
        S.lineHeight(1.12),
        S.margin(0.px),
      )

  object Display
      extends CssClass(
        S.fontSize.px(28),
        S.fontWeight(650),
        S.margin(0.px),
        S.lineHeight(1.2),
      )

  object Mark
      extends CssClass(
        S.height.px(36),
        S.width.px(36),
        S.borderRadius.px(8),
        S.display.block,
      )

  object People
      extends CssClass(
        S.display.grid,
        Declaration("grid-template-columns", "repeat(auto-fit, minmax(9.5rem, 1fr))"),
        S.gap(0.5.rem),
      )

  object NavLink
      extends CssClass(
        S.color(ink),
        S.textDecoration.none,
        S.fontWeight(650),
      )

  object Door
      extends CssClass(
        S.display.inlineFlex,
        S.alignItems.center,
        S.padding(0.55.rem, 0.95.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(8),
        S.background(accent),
        S.color(Color.hex("#f7f1ea")),
        S.fontWeight(700),
        S.fontSize.px(14),
        S.textDecoration.none,
        Declaration("font-family", font),
      )

  object DoorGhost
      extends CssClass(
        S.display.inlineFlex,
        S.alignItems.center,
        S.padding(0.55.rem, 0.95.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.background(panel),
        S.color(ink),
        S.fontWeight(650),
        S.fontSize.px(14),
        S.textDecoration.none,
        Declaration("font-family", font),
      )

  object Bar
      extends CssClass(
        S.display.flex,
        S.flexWrap.wrap,
        S.alignItems.center,
        S.justifyContent.spaceBetween,
        S.gap(0.75.rem),
      )

  object Article
      extends CssClass(
        S.display.grid,
        S.gap(1.05.rem),
        S.color(ink),
        S.fontFamily(FontFamily.Single("var(--specular-font-sans)")),
        S.fontSize.px(16),
        S.lineHeight(1.5),
        S.width.pct(100),
        S.minWidth.px(0),
      )

  object Lead
      extends CssClass(
        S.margin.px(0),
        S.fontSize.px(18),
        S.lineHeight(1.45),
      )

  object Copy
      extends CssClass(
        S.margin.px(0),
        S.color(ink),
      )

  object H2
      extends CssClass(
        S.margin(1.15.rem, 0.px, 0.px, 0.px),
        S.fontSize.px(20),
        S.fontWeight(650),
        S.lineHeight(1.3),
      )

  object Bullets
      extends CssClass(
        S.margin.px(0),
        S.padding(0.px, 0.px, 0.px, 1.2.rem),
        S.display.grid,
        S.gap(0.4.rem),
      )

  def mount(children: UI[Any]*): UI[Any] =
    E.div(Article, UI.Fragment(children.toVector))

  object Shell
      extends CssClass(
        S.display.grid,
        S.gap(1.rem),
        S.padding(1.15.rem, 1.25.rem),
        S.background(panel),
        S.border(Border.solid(1.px, line)),
        S.borderRadius(radius),
        S.color(ink),
        Declaration("font-family", font),
        S.fontSize.px(15),
        S.lineHeight(1.45),
      )

  object Poster
      extends CssClass(
        S.display.grid,
        S.gap(1.rem),
        S.padding(1.25.rem),
        S.background(code),
        S.border(Border.solid(1.px, line)),
        S.borderRadius(radius),
        S.color(ink),
        Declaration("font-family", font),
      )

  object Stack
      extends CssClass(
        S.display.grid,
        S.gap(0.75.rem),
        S.minWidth.px(0),
      )

  object Row
      extends CssClass(
        S.display.flex,
        S.flexWrap.wrap,
        S.alignItems.center,
        S.gap(0.55.rem),
      )

  object Hosts
      extends CssClass(
        S.display.grid,
        Declaration("grid-template-columns", "repeat(auto-fit, minmax(9.5rem, 1fr))"),
        S.gap(0.65.rem),
      )

  object Card
      extends CssClass(
        S.display.grid,
        S.gap(0.35.rem),
        S.padding(0.85.rem, 0.95.rem),
        S.background(panel),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(10),
        S.color(ink),
        S.minWidth.px(0),
      )

  object CardOn
      extends CssClass(
        S.display.grid,
        S.gap(0.35.rem),
        S.padding(0.85.rem, 0.95.rem),
        S.background(panel),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(10),
        S.color(ink),
        S.boxShadow(Shadow(0.px, 0.px, 0.px, 2.px, ring.alpha(0.45))),
        S.minWidth.px(0),
      )

  object CardGhost
      extends CssClass(
        S.display.grid,
        S.gap(0.35.rem),
        S.padding(0.85.rem, 0.95.rem),
        S.background(panel),
        S.border(Border.dashed(1.px, line)),
        S.borderRadius.px(10),
        S.color(muted),
        S.opacity(0.72),
        S.minWidth.px(0),
      )

  object Bound
      extends CssClass(
        S.display.grid,
        S.gap(0.4.rem),
        S.padding(1.1.rem, 1.2.rem),
        S.background(panel),
        S.border(Border.solid(2.px, accent)),
        S.borderRadius.px(12),
        S.textAlign.center,
      )

  object Kicker
      extends CssClass(
        S.fontSize.px(11),
        S.letterSpacing(0.08.em),
        S.textTransform.uppercase,
        S.color(muted),
        S.fontWeight(600),
      )

  object Title
      extends CssClass(
        S.fontSize.px(18),
        S.fontWeight(650),
        S.margin(0.px),
        S.lineHeight(1.25),
      )

  object Fine
      extends CssClass(
        S.fontSize.px(13),
        S.color(muted),
        S.margin(0.px),
        S.lineHeight(1.4),
      )

  object Chip
      extends CssClass(
        S.display.inlineFlex,
        S.alignItems.center,
        S.gap(0.3.rem),
        S.padding(0.2.rem, 0.55.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(999),
        S.fontSize.px(12),
        S.fontWeight(600),
        S.letterSpacing(0.02.em),
        S.background(board),
        S.color(ink),
      )

  object ChipOn
      extends CssClass(
        S.display.inlineFlex,
        S.alignItems.center,
        S.gap(0.3.rem),
        S.padding(0.2.rem, 0.55.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(999),
        S.fontSize.px(12),
        S.fontWeight(700),
        S.background(accent),
        S.color(Color.hex("#f7f1ea")),
      )

  object ChipGhost
      extends CssClass(
        S.display.inlineFlex,
        S.alignItems.center,
        S.gap(0.3.rem),
        S.padding(0.2.rem, 0.55.rem),
        S.border(Border.dashed(1.px, line)),
        S.borderRadius.px(999),
        S.fontSize.px(12),
        S.fontWeight(600),
        S.color(muted),
      )

  object Method
      extends CssClass(
        S.display.inlineBlock,
        S.padding(0.12.rem, 0.45.rem),
        S.borderRadius.px(4),
        S.background(accent),
        S.color(Color.hex("#f7f1ea")),
        S.fontSize.px(11),
        S.fontWeight(700),
        S.letterSpacing(0.04.em),
      )

  object Btn
      extends CssClass(
        S.padding(0.4.rem, 0.8.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.background(board),
        S.color(ink),
        S.cursor.pointer,
        S.fontWeight(600),
        S.fontSize.px(13),
        Declaration("font-family", font),
        S.transition("border-color 120ms ease, background-color 120ms ease"),
        Selector(PseudoClass.hover, S.borderColor(accent)),
      )

  object BtnOn
      extends CssClass(
        S.padding(0.4.rem, 0.8.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(8),
        S.background(accent),
        S.color(Color.hex("#f7f1ea")),
        S.cursor.pointer,
        S.fontWeight(700),
        S.fontSize.px(13),
        Declaration("font-family", font),
      )

  object Mono
      extends CssClass(
        Declaration("font-family", "ui-monospace, 'SF Mono', Menlo, Consolas, monospace"),
        S.fontSize.px(13),
        S.whiteSpace.preWrap,
        S.background(code),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.padding(0.85.rem, 1.rem),
        S.margin(0.px),
        S.overflowX.auto,
        S.color(codeInk),
        S.lineHeight(1.45),
      )

  /** Host payloads share one height so cycling HTTP / OpenAPI / MCP / stdio cannot reflow the page. */
  object Payload
      extends CssClass(
        Declaration("font-family", "ui-monospace, 'SF Mono', Menlo, Consolas, monospace"),
        S.fontSize.px(13),
        S.whiteSpace.preWrap,
        S.background(code),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.padding(0.85.rem, 1.rem),
        S.margin(0.px),
        S.overflowX.auto,
        S.overflowY.auto,
        S.color(codeInk),
        S.lineHeight(1.45),
        S.minHeight.px(188),
        S.boxSizing.borderBox,
      )

  object Field
      extends CssClass(
        S.display.grid,
        S.gap(0.15.rem),
        S.padding(0.55.rem, 0.65.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.background(board),
        S.cursor.pointer,
        S.textAlign.left,
        Declaration("font-family", font),
        S.color(ink),
        S.transition("border-color 120ms ease"),
        Selector(PseudoClass.hover, S.borderColor(link)),
      )

  object FieldOn
      extends CssClass(
        S.display.grid,
        S.gap(0.15.rem),
        S.padding(0.55.rem, 0.65.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(8),
        S.background(panel),
        S.cursor.pointer,
        S.textAlign.left,
        Declaration("font-family", font),
        S.color(ink),
        S.boxShadow(Shadow(0.px, 0.px, 0.px, 2.px, ring.alpha(0.4))),
      )

  object StepOn
      extends CssClass(
        S.display.grid,
        S.gap(0.2.rem),
        S.padding(0.7.rem, 0.8.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(8),
        S.background(panel),
        S.cursor.pointer,
        S.textAlign.left,
        Declaration("font-family", font),
        S.color(ink),
      )

  object StepOff
      extends CssClass(
        S.display.grid,
        S.gap(0.2.rem),
        S.padding(0.7.rem, 0.8.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.background(board),
        S.opacity(0.7),
        S.cursor.pointer,
        S.textAlign.left,
        Declaration("font-family", font),
        S.color(ink),
      )

  object Hint
      extends CssClass(
        S.color(muted),
        S.fontSize.px(13),
        S.lineHeight(1.4),
        S.margin(0.px),
      )

  object Ok
      extends CssClass(
        S.color(chalkOk),
        S.fontWeight(650),
      )

  object Person
      extends CssClass(
        S.display.flex,
        S.alignItems.center,
        S.justifyContent.spaceBetween,
        S.gap(0.75.rem),
        S.padding(0.7.rem, 0.85.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.background(board),
        S.cursor.pointer,
        Declaration("font-family", font),
        S.color(ink),
        Selector(PseudoClass.hover, S.borderColor(accent)),
      )

  def chip(label: String, on: Boolean = false, ghost: Boolean = false): UI[Any] =
    val cls = if ghost then ChipGhost else if on then ChipOn else Chip
    E.span(cls, label)

  def method(name: String): UI[Any] =
    E.span(Method, name)

  def modeButton(mode: Source[String], label: String): UI[Any] =
    E.button(
      mode.map(m => if m == label then Set[CssClass](BtnOn) else Set[CssClass](Btn)),
      Events.onClick(_ => mode.set(label)),
      label,
    )

  def kicker(text: String): UI[Any] =
    E.div(Kicker, text)

  def hostCard(title: String, body: String, on: Boolean = false, ghost: Boolean = false): UI[Any] =
    val cls = if ghost then CardGhost else if on then CardOn else Card
    E.div(cls, E.div(Kicker, title), E.p(Fine, body))

  def poster: UI[Any] =
    posterAt("HTTP", display = false, live = None)

  def posterAt(host: String, display: Boolean, live: Option[(Source[String], Source[Boolean])]): UI[Any] =
    val name = if display then Display else Title
    E.div(
      Poster,
      kicker("capability compiler"),
      E.div(
        Bound,
        kicker("BoundOp"),
        E.h3(name, "getUser"),
        E.p(Fine, "Endpoint + In => ZIO[R, E, Out]. Written once."),
        E.div(Row, method("GET"), chip("/users/{id}"), chip(".mcp"), chip("ReadOnly")),
      ),
      E.div(
        Hosts,
        live match
          case Some((src, pin)) =>
            UI.Fragment(
              Vector(
                hostTile(src, pin, "HTTP", "api.routes. Humans and systems."),
                hostTile(src, pin, "OpenAPI", "api.openApi. Swagger at /docs."),
                hostTile(src, pin, "MCP", "Mcp.from(api). POST /mcp."),
                hostTile(src, pin, "stdio", "mcp.stdio(). Local harnesses."),
                hostCard("CLI", "Same OpArgs. Not shipped. The AST is already this shape.", ghost = true),
              )
            )
          case None =>
            UI.Fragment(
              Vector(
                hostCard("HTTP", "api.routes. Humans and systems.", on = host == "HTTP"),
                hostCard("OpenAPI", "api.openApi. Swagger at /docs.", on = host == "OpenAPI"),
                hostCard("MCP HTTP", "Mcp.from(api). POST /mcp.", on = host == "MCP"),
                hostCard("stdio", "mcp.stdio(). Local harnesses.", on = host == "stdio"),
                hostCard("CLI", "Same OpArgs. Not shipped. The AST is already this shape.", ghost = true),
              )
            ),
      ),
      E.pre(Payload, hostBody(host)),
      E.p(Hint, "Hosts are interpreters. Do not grow a second tool DSL."),
    )
  end posterAt

  def hostTile(host: Source[String], pinned: Source[Boolean], label: String, body: String): UI[Any] =
    E.button(
      host.map(h => if h == label then Set[CssClass](CardOn) else Set[CssClass](Card)),
      Events.onClick(_ => pinned.set(true) *> host.set(label)),
      E.div(Kicker, label),
      E.p(Fine, body),
    )

  def hostBody(host: String, p: Person = people.head): String =
    host match
      case "OpenAPI" =>
        s"""/users/{id}:
  get:
    operationId: get_users_id
    summary: Get a user
    responses:
      "200": { schema: User }
      "404": { schema: NotFound }"""
      case "MCP" =>
        s"""POST /mcp
{"method":"tools/call",
 "params":{"name":"get_users_id","arguments":{"id":${p.id}}}}

→ {"id":${p.id},"name":"${p.name}"}"""
      case "stdio" =>
        s"""stdin  → tools/call get_users_id {"id":${p.id}}
stdout ← {"id":${p.id},"name":"${p.name}"}

one JSON-RPC line in, one line out."""
      case _ =>
        s"""GET /users/${p.id} HTTP/1.1

HTTP/1.1 200 OK
content-type: application/json

{"id":${p.id},"name":"${p.name}"}"""

  def schemaPoster: UI[Any] =
    schemaPosterAt("JSON")

  def schemaPosterAt(selected: String): UI[Any] =
    val body = schemaBody(selected)
    E.div(
      Poster,
      kicker("one type, three readers · click a host"),
      E.div(Bound, kicker("Schema"), E.h3(Title, "User"), E.p(Fine, "id: Int, name: String")),
      E.div(
        Hosts,
        hostCard("JSON", """{"id":1,"name":"Ada"}""", on = selected == "JSON"),
        hostCard("OpenAPI", "components.schemas.User", on = selected == "OpenAPI"),
        hostCard("MCP args", "inputSchema for get_users_id", on = selected == "MCP args"),
        hostCard("CLI flags", "--id 1   (later interpreter)", ghost = true),
      ),
      E.pre(Payload, body),
    )
  end schemaPosterAt

  def schemaBody(selected: String): String =
    selected match
      case "OpenAPI"  => "components.schemas.User"
      case "MCP args" => "get_users_id inputSchema  { id: integer }"
      case "CLI"      => "--id 1   (later interpreter)"
      case _          => """{"id":1,"name":"Ada"}"""

  def schemaPosterLive(selected: Source[String]): UI[Any] =
    E.div(
      Poster,
      kicker("one type, three readers · click a host"),
      E.div(Bound, kicker("Schema"), E.h3(Title, "User"), E.p(Fine, "id: Int, name: String")),
      E.div(
        Hosts,
        hostTile(selected, "JSON", """{"id":1,"name":"Ada"}"""),
        hostTile(selected, "OpenAPI", "components.schemas.User"),
        hostTile(selected, "MCP args", "inputSchema for get_users_id"),
        hostCard("CLI flags", "--id 1   (later interpreter)", ghost = true),
      ),
      selected.map(s => E.pre(Payload, schemaBody(s))),
    )

  def hostTile(selected: Source[String], label: String, body: String): UI[Any] =
    E.button(
      selected.map(s => if s == label then Set[CssClass](CardOn) else Set[CssClass](Card)),
      Events.onClick(_ => selected.set(label)),
      E.div(Kicker, label),
      E.p(Fine, body),
    )

  final case class OpField(id: String, label: String, value: String, readers: String)

  val getUserFields: List[OpField] = List(
    OpField("method", "method", "GET", "HTTP, OpenAPI, derived tool name"),
    OpField("path", "path", "/users/{id}", "HTTP route, OpenAPI path, OpArgs path fill"),
    OpField("in", "in", "id: Int", "decodeIn, MCP argument, future CLI flag"),
    OpField("out", "out", "User @ 200", "encodeOut, OpenAPI response, tool result"),
    OpField("err", "error", "NotFound @ 404", "encodeErr, OpenAPI, MCP isError"),
    OpField("mcp", ".mcp", "promoted get_users_id", "Mcp.from tools/list"),
    OpField("hint", "hints", "ReadOnly", "MCP tool annotations"),
  )

  def opAnatomy(selected: String): UI[Any] =
    val field = getUserFields.find(_.id == selected).getOrElse(getUserFields.head)
    opAnatomyView(
      field,
      getUserFields.map { f =>
        E.div(
          if f.id == field.id then FieldOn else Field,
          E.div(Kicker, f.label),
          E.div(f.value),
        )
      },
    )
  end opAnatomy

  def opAnatomyLive(selected: Source[String]): UI[Any] =
    val field = selected.map(id => getUserFields.find(_.id == id).getOrElse(getUserFields.head))
    E.div(
      Shell,
      kicker("EndpointDoc · click a field"),
      E.div(Row, method("GET"), E.strong("/users/{id}"), chip(".mcp", on = true), chip("ReadOnly")),
      E.div(
        Hosts,
        UI.Fragment(
          getUserFields.map { f =>
            E.button(
              selected.map(id => if id == f.id then Set[CssClass](FieldOn) else Set[CssClass](Field)),
              Events.onClick(_ => selected.set(f.id)),
              E.div(Kicker, f.label),
              E.div(f.value),
            )
          }.toVector
        ),
      ),
      E.div(CardOn, kicker("read by"), E.p(Fine, field.map(_.readers))),
    )
  end opAnatomyLive

  private def opAnatomyView(field: OpField, fields: List[UI[Any]]): UI[Any] =
    E.div(
      Shell,
      kicker("EndpointDoc"),
      E.div(Row, method("GET"), E.strong("/users/{id}"), chip(".mcp", on = true), chip("ReadOnly")),
      E.div(Hosts, UI.Fragment(fields.toVector)),
      E.div(CardOn, kicker("read by"), E.p(Fine, field.readers)),
    )

  val traceSteps: List[(String, String)] = List(
    "request" -> "GET /users/1 arrives. Path codec captures id = 1.",
    "decode"  -> "decodeIn yields In = 1. Missing/invalid path is already 404.",
    "run"     -> "BoundOp.run: ZIO[Any, NotFound, User]. Ada is in the store.",
    "encode"  -> "encodeOut writes application/json. Fiber exits with the connection.",
  )

  def traceSnippet(step: Int): String =
    step match
      case 0 => "Request.get(\"/users/1\")"
      case 1 => "path.matches → Some(1)"
      case 2 => "store.get.map(_.get(1))  // User(1, Ada)"
      case _ => "200  {\"id\":1,\"name\":\"Ada\"}"

  def effectTrace(step: Int): UI[Any] =
    val i = step.max(0).min(traceSteps.length - 1)
    E.div(
      Shell,
      kicker("the effect is the body"),
      E.div(
        Hosts,
        UI.Fragment(
          traceSteps.zipWithIndex.map { case ((name, _), n) =>
            E.div(if n == i then StepOn else StepOff, kicker(s"0${n + 1}"), E.strong(name))
          }.toVector
        ),
      ),
      E.p(Fine, traceSteps(i)._2),
      E.pre(Mono, traceSnippet(i)),
    )
  end effectTrace

  def effectTraceLive(step: Source[Int]): UI[Any] =
    E.div(
      Shell,
      kicker("the effect is the body · click a step"),
      E.div(
        Hosts,
        UI.Fragment(
          traceSteps.zipWithIndex.map { case ((name, _), n) =>
            E.button(
              step.map(i => if i == n then Set[CssClass](StepOn) else Set[CssClass](StepOff)),
              Events.onClick(_ => step.set(n)),
              kicker(s"0${n + 1}"),
              E.strong(name),
            )
          }.toVector
        ),
      ),
      step.map { n =>
        val i = n.max(0).min(traceSteps.length - 1)
        E.div(Stack, E.p(Fine, traceSteps(i)._2), E.pre(Mono, traceSnippet(i)))
      },
    )
  end effectTraceLive

  def hostFanout(host: String): UI[Any] =
    E.div(
      Shell,
      kicker("same BoundOp"),
      E.div(Row, E.strong("get_users_id"), chip("ReadOnly", on = true)),
      E.pre(Payload, hostBody(host)),
      E.p(Hint, "HTTP, OpenAPI, MCP, and stdio are hosts. They are not copies."),
    )

  def swaggerWalk: UI[Any] =
    swaggerWalkAt(ran = false)

  def swaggerWalkAt(ran: Boolean): UI[Any] =
    E.div(
      Shell,
      kicker("OpenAPI · Try it out"),
      E.div(Row, method("GET"), E.strong("/users/{id}"), chip("users")),
      E.div(Card, kicker("parameters"), E.div("id · path · integer = 1")),
      E.div(Row, E.span(if ran then ChipOn else Chip, "Execute")),
      if ran then E.div(CardOn, kicker("200 OK"), E.pre(Mono, """{"id":1,"name":"Ada"}"""))
      else E.p(Hint, "Execute runs GET /users/1."),
      E.p(Hint, "api.openApi.routes(\"docs\") serves this. No second spec."),
    )

  def swaggerWalkLive(ran: Source[Boolean]): UI[Any] =
    E.div(
      Shell,
      kicker("OpenAPI · Try it out"),
      E.div(Row, method("GET"), E.strong("/users/{id}"), chip("users")),
      E.div(Card, kicker("parameters"), E.div("id · path · integer = 1")),
      E.button(BtnOn, Events.onClick(_ => ran.set(true)), "Execute"),
      ran.map { ok =>
        if ok then E.div(CardOn, kicker("200 OK"), E.pre(Mono, """{"id":1,"name":"Ada"}"""))
        else E.p(Hint, "Execute runs GET /users/1.")
      },
      E.p(Hint, "api.openApi.routes(\"docs\") serves this. No second spec."),
    )

  def harnessWalk(phase: String): UI[Any] =
    val body = phase match
      case "call" =>
        """Grok  →  tools/call  get_users_id  {id: 1}
hub   ←  {"id":1,"name":"Ada"}

Same function as GET /users/1."""
      case _ =>
        """Grok  →  tools/list
hub   ←  get_users_id   GET /users/{id}  ReadOnly
         get_users      GET /users       ReadOnly

createUser is not promoted. Catalog can still find it."""
    E.div(
      Shell,
      kicker("harness · Grok Build"),
      E.pre(Mono, body),
      E.p(Hint, "Same bind the tests run. Clone for a JVM process."),
    )
  end harnessWalk

  def desk(selected: Option[Int], host: String = "HTTP"): UI[Any] =
    val person = selected.flatMap(id => people.find(_.id == id))
    E.div(
      Shell,
      kicker("directory · a web client"),
      E.div(
        People,
        UI.Fragment(
          people.map { p =>
            E.div(
              if selected.contains(p.id) then CardOn else Card,
              kicker(s"GET /users/${p.id}"),
              E.strong(p.name),
            )
          }.toVector
        ),
      ),
      person match
        case Some(p) =>
          E.div(
            Stack,
            E.div(CardOn, kicker(s"GET /users/${p.id} is get_users_id"), E.h3(Title, p.name), E.p(Fine, p.role)),
            E.pre(Payload, hostBody(host, p)),
          )
        case None =>
          E.p(Hint, "pick a person. GET /users/{id} is get_users_id."),
    )
  end desk

  def deskLive(selected: Source[Int], host: Source[String]): UI[Any] =
    E.div(
      Shell,
      kicker("directory · a web client"),
      E.div(
        People,
        UI.Fragment(
          people.map { p =>
            E.button(
              selected.map(id => if id == p.id then Set[CssClass](CardOn) else Set[CssClass](Card)),
              Events.onClick(_ => selected.set(p.id)),
              kicker(s"GET /users/${p.id}"),
              E.strong(p.name),
            )
          }.toVector
        ),
      ),
      selected.map { id =>
        val p = people.find(_.id == id).getOrElse(people.head)
        E.div(
          Stack,
          E.div(CardOn, kicker(s"GET /users/${p.id} is get_users_id"), E.h3(Title, p.name), E.p(Fine, p.role)),
          E.div(
            Row,
            modeButton(host, "HTTP"),
            modeButton(host, "OpenAPI"),
            modeButton(host, "MCP"),
          ),
          host.map(h => E.pre(Payload, hostBody(h, p))),
        )
      },
    )
  end deskLive

  def pathPlay(path: String): UI[Any] =
    val (ok, note) = path match
      case "/users/1"   => (true, "match  id = 1")
      case "/users/42"  => (true, "match  id = 42")
      case "/users/ada" => (false, "int(\"id\") rejects non-digits → 404")
      case "/users"     => (false, "missing capture → 404")
      case other        => (false, s"no match  $other")
    E.div(
      Shell,
      kicker("PathCodec"),
      E.div(Row, method("GET"), E.code(""" "users" / int("id") """)),
      E.pre(Mono, s"$path\n$note"),
      E.p(if ok then Ok else Hint, if ok then "handler runs" else "unmatched. 404."),
    )
  end pathPlay

  def middleware(order: String): UI[Any] =
    val (code, walk) = order match
      case "cors-id" =>
        (
          "routes @@ (Middleware.cors() ++ Middleware.requestId())",
          "request → requestId → cors → handler\n`a ++ b` applies a, then wraps with b. Incoming hits b first.",
        )
      case _ =>
        (
          "routes @@ (Middleware.requestId() ++ Middleware.cors())",
          "request → cors → requestId → handler\ncors is outer. requestId still stamps X-Request-Id.",
        )
    E.div(Shell, kicker("Middleware @@"), E.pre(Mono, s"$code\n\n$walk"))
  end middleware

  def catalog(mode: String): UI[Any] =
    val body =
      if mode == "catalog" then """tools/list  (withCatalog)
- get_users_id      GET /users/{id}   ReadOnly
- get_users         GET /users        ReadOnly
- search_operations
- invoke            # post_users lives here"""
      else """tools/list  (promoted only)
- get_users_id      GET /users/{id}   ReadOnly
- get_users         GET /users        ReadOnly

createUser has no .mcp."""
    E.div(Shell, kicker("promote, don't auto-export"), E.pre(Mono, body))
  end catalog

  def rpc(side: String): UI[Any] =
    val body =
      if side == "response" then """{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{ "type": "text", "text": "{\"id\":1,\"name\":\"Ada\"}" }]
  }
}"""
      else """{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "get_users_id",
    "arguments": { "id": 1 }
  }
}"""
    E.div(
      Shell,
      kicker("same function as GET /users/1"),
      E.pre(Mono, body),
    )
  end rpc

  def sseTape(n: Int): UI[Any] =
    val events = (0 to n.max(0)).map(i => s"event: tick\nid: $i\ndata: $i").mkString("\n\n")
    E.div(
      Shell,
      kicker("Sse.response"),
      E.pre(Mono, events),
      E.p(Hint, "Each ServerSentEvent is its own HTTP chunk. The fiber dies with the connection."),
    )

  def datastar(kind: String): UI[Any] =
    val body =
      if kind == "signals" then """event: datastar-patch-signals
data: signals {"count":1}"""
      else """event: datastar-patch-elements
data: selector #app
data: mode inner
data: elements <div>hello</div>"""
    E.div(Shell, kicker("heddle.datastar"), E.pre(Mono, body))

  def openApi(op: String): UI[Any] =
    val yaml = op match
      case "post" =>
        """/users:
  post:
    summary: Create a user
    requestBody: { schema: NewUser }
    responses: { "201": { schema: User } }"""
      case "list" =>
        """/users:
  get:
    summary: List users
    responses: { "200": { schema: User[] } }"""
      case _ =>
        """/users/{id}:
  get:
    summary: Get a user
    parameters: [{ name: id, in: path, schema: integer }]
    responses:
      "200": { schema: User }
      "404": { schema: NotFound }"""
    E.div(
      Shell,
      kicker("OpenAPI is a projection"),
      E.pre(Mono, yaml),
    )
  end openApi

  def posterLive(host: Source[String], pinned: Source[Boolean]): UI[Any] =
    E.div(
      Poster,
      kicker("capability compiler"),
      E.div(
        Bound,
        kicker("BoundOp"),
        E.h3(Display, "getUser"),
        E.p(Fine, "Endpoint + In => ZIO[R, E, Out]. Written once."),
        E.div(Row, method("GET"), chip("/users/{id}"), chip(".mcp"), chip("ReadOnly")),
      ),
      E.div(
        Hosts,
        hostTile(host, pinned, "HTTP", "api.routes. Humans and systems."),
        hostTile(host, pinned, "OpenAPI", "api.openApi. Swagger at /docs."),
        hostTile(host, pinned, "MCP", "Mcp.from(api). POST /mcp."),
        hostTile(host, pinned, "stdio", "mcp.stdio(). Local harnesses."),
        hostCard("CLI", "Same OpArgs. Not shipped. The AST is already this shape.", ghost = true),
      ),
      host.map(h => E.pre(Payload, hostBody(h))),
      E.p(Hint, "Hosts are interpreters. Do not grow a second tool DSL."),
    )

  /** Interactive trees for `illustrationIO { ... }.live`. Shared by JVM SSR and the JS client. */
  object Lives:
    def landingPoster: URIO[Scope, UI[Any]] =
      val cycle = Vector("HTTP", "OpenAPI", "MCP", "stdio")
      for
        host   <- sq("HTTP")
        pinned <- sq(false)
        _      <- (ZIO.sleep(2.seconds) *>
          pinned.get.flatMap {
            case true  => ZIO.unit
            case false =>
              host.get.flatMap { h =>
                val i = math.max(0, cycle.indexOf(h))
                host.set(cycle((i + 1) % cycle.length))
              }
          }).forever.forkScoped
      yield posterLive(host, pinned)
      end for
    end landingPoster

    def hostFanout: URIO[Scope, UI[Any]] =
      for host <- sq("HTTP")
      yield E.div(
        Stack,
        kicker("same BoundOp · get_users_id"),
        E.div(
          Row,
          modeButton(host, "HTTP"),
          modeButton(host, "OpenAPI"),
          modeButton(host, "MCP"),
          modeButton(host, "stdio"),
        ),
        host.map(Hub.hostFanout),
      )

    def opAnatomy: URIO[Scope, UI[Any]] =
      for selected <- sq("method")
      yield opAnatomyLive(selected)

    def effectTrace: URIO[Scope, UI[Any]] =
      for step <- sq(0)
      yield effectTraceLive(step)

    def swaggerWalk: URIO[Scope, UI[Any]] =
      for ran <- sq(false)
      yield swaggerWalkLive(ran)

    def schemaPoster: URIO[Scope, UI[Any]] =
      for selected <- sq("JSON")
      yield schemaPosterLive(selected)

    def harnessWalk: URIO[Scope, UI[Any]] =
      for phase <- sq("list")
      yield E.div(
        Stack,
        E.div(Row, modeButton(phase, "list"), modeButton(phase, "call")),
        phase.map(Hub.harnessWalk),
      )

    def desk: URIO[Scope, UI[Any]] =
      for
        selected <- sq(1)
        host     <- sq("HTTP")
      yield deskLive(selected, host)

    def pathPlay: URIO[Scope, UI[Any]] =
      for path <- sq("/users/1")
      yield E.div(
        Stack,
        E.div(
          Row,
          modeButton(path, "/users/1"),
          modeButton(path, "/users/42"),
          modeButton(path, "/users/ada"),
          modeButton(path, "/users"),
        ),
        path.map(Hub.pathPlay),
      )

    def middleware: URIO[Scope, UI[Any]] =
      for order <- sq("id-cors")
      yield E.div(
        Stack,
        E.div(Row, modeButton(order, "id-cors"), modeButton(order, "cors-id")),
        order.map(Hub.middleware),
      )

    def openApi: URIO[Scope, UI[Any]] =
      for op <- sq("get")
      yield E.div(
        Stack,
        E.div(Row, modeButton(op, "get"), modeButton(op, "list"), modeButton(op, "post")),
        op.map(Hub.openApi),
      )

    def catalog: URIO[Scope, UI[Any]] =
      for mode <- sq("promoted")
      yield E.div(
        Stack,
        E.div(Row, modeButton(mode, "promoted"), modeButton(mode, "catalog")),
        mode.map(Hub.catalog),
      )

    def rpc: URIO[Scope, UI[Any]] =
      for side <- sq("request")
      yield E.div(
        Stack,
        E.div(Row, modeButton(side, "request"), modeButton(side, "response")),
        side.map(Hub.rpc),
      )

    def sseTape: URIO[Scope, UI[Any]] =
      for n <- sq(0)
      yield E.div(
        Stack,
        E.div(
          Row,
          E.button(Btn, Events.onClick(_ => n.update(_ + 1)), "next event"),
          E.span(Hint, n.map(i => s"id $i")),
        ),
        n.map(Hub.sseTape),
      )

    def datastar: URIO[Scope, UI[Any]] =
      for kind <- sq("elements")
      yield E.div(
        Stack,
        E.div(Row, modeButton(kind, "elements"), modeButton(kind, "signals")),
        kind.map(Hub.datastar),
      )
  end Lives
end Hub
