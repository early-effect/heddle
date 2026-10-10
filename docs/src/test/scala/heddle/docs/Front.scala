package heddle.docs

import heddle.*
import heddle.docs.ui.Hub
import mermoid.{LayoutConfig, Mermaid, RenderConfig, SequenceConfig}
import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Install coordinate. Judge the raw build version. Do not trust `displayVersion` after `-ci` was stripped. */
object DocsVersion:
  /** Last published tag. */
  val published = "0.10.0"

  /** A dynver distance (`-ci`, `+`, `SNAPSHOT`) is the next line. Anything else is advertised as written. */
  def advertise(raw: String): String =
    val trimmed  = raw.trim
    val distance = trimmed.contains("-ci") || trimmed.contains("+") || trimmed.contains("SNAPSHOT")
    if trimmed.nonEmpty && !distance then trimmed else published

  /** `specular.meta.version` is the raw build version. Absent in unit tests, so those use [[published]]. */
  def version: String =
    sys.props.get("specular.meta.version").map(advertise).getOrElse(published)
end DocsVersion

/** The site front. Specular still emits a summary index; [[BuildSite]] copies this page over it. */
object Front extends DocSpecSuite:

  private val coordinate: String =
    s"""libraryDependencies += "rocks.earlyeffect" %% "heddle" % "${DocsVersion.version}""""

  /** Columns put HTTP between Caller and BoundOp so `implement` is not an adjacent label. An adjacent label is as wide
    * as its text, and five full names in a row do not fit a phone.
    */
  private val hosts =
    Mermaid("""sequenceDiagram
              |    participant Caller
              |    participant HTTP
              |    participant BoundOp
              |    participant OpenAPI
              |    participant MCP
              |    Caller->>BoundOp: implement
              |    BoundOp->>HTTP: routes
              |    BoundOp-->>OpenAPI: doc
              |    MCP->>BoundOp: tools/call
              |""".stripMargin)

  private val diagramConfig =
    RenderConfig(
      layout = LayoutConfig(padding = 8, fontSize = 12, edgeLabelFontSize = 11, lineHeight = 14),
      sequence = SequenceConfig(
        actorMinWidth = 32,
        actorPadH = 4,
        actorHeight = 28,
        columnGap = 8,
        rowPitch = 36,
        headerGap = 8,
        footerGap = 8,
      ),
    )

  /** Phone column is about 390px. A figure wider than the content box scrolls. */
  private def fits(text: String): Boolean =
    svgWidth(text).exists(_ <= 360)

  private def svgWidth(text: String): Option[Double] =
    val attrs = List(
      """width,Str\(([0-9.]+)\)""".r,
      """width="([0-9.]+)"""".r,
      """data-mermoid-width="([0-9.]+)"""".r,
      """data-mermoid-width,Str\(([0-9.]+)\)""".r,
    )
    attrs.flatMap(_.findAllMatchIn(text)).flatMap(m => m.group(1).toDoubleOption).maxOption

  def doc = page("One BoundOp, three hosts")(
    md"""One BoundOp. HTTP, OpenAPI, and MCP are hosts.""",
    illustration(MermoidAscent.svgDiagram(hosts, diagramConfig)).assert { ui =>
      val text = ui.toString
      assertTrue(
        text.contains("Caller"),
        text.contains("BoundOp"),
        text.contains("HTTP"),
        text.contains("OpenAPI"),
        text.contains("MCP"),
        text.contains("implement"),
        text.contains("routes"),
        text.contains("tools/call"),
      )
    },
    md"""
`implement` returns the BoundOp. `routes` is the HTTP host. The dashed `doc` is OpenAPI: `Api.openApi` reads `endpoint.doc`, and that document is not a second runtime. `Mcp.from` turns a promoted operation into a tool, and `tools/call` runs the same function.
""",
    section("Install")(
      md"""
```scala
${coordinate}
```
"""
    ),
    section("Endpoint.mcp")(
      md"""
`Endpoint.get` is inline, so a cite expands the macro, and a call is not a definition. `mcp` sets `doc.promoted`.
""",
      cite[Endpoint[Unit, Nothing, Unit]](_.mcp),
    ),
    section("get_show")(
      md"""The buttons replay one bound operation. They do not call a server.""",
      illustrationIO(Hub.Lives.hostFanout).live
        .withMountKey(InteractiveRegistry.FrontHosts)
        .assert { ui =>
          val text = ui.toString
          assertTrue(text.contains("get_show"), text.contains("HTTP"), text.contains("OpenAPI"), text.contains("MCP"))
        },
    ),
  )

  override def spec =
    suite("One BoundOp, three hosts")(
      super.spec,
      test("install coordinate is the published release") {
        val version = DocsVersion.version
        assertTrue(
          coordinate.contains(version),
          coordinate.contains("rocks.earlyeffect"),
          coordinate.contains("%% \"heddle\""),
          !version.contains("-ci"),
          !version.contains("SNAPSHOT"),
          !version.contains("+"),
          !coordinate.contains("-ci"),
          !coordinate.contains("SNAPSHOT"),
          !coordinate.contains("0.11.0"),
          DocsVersion.advertise("0.11.0-ci") == DocsVersion.published,
          DocsVersion.advertise("0.10.0+7-abcdef") == DocsVersion.published,
          DocsVersion.advertise("0.11.0-SNAPSHOT") == DocsVersion.published,
          DocsVersion.advertise("0.10.0") == "0.10.0",
        )
      },
      test("sequence diagram fits a phone column") {
        val svg   = MermoidAscent.svg(hosts, diagramConfig)
        val width = svgWidth(svg)
        assertTrue(
          fits(svg),
          svg.contains("BoundOp"),
          svg.contains("OpenAPI"),
          svg.contains("implement"),
          svg.contains("tools/call"),
        ).label(s"svg width $width")
      },
    )
end Front
