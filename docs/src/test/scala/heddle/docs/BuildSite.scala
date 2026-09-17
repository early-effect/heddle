package heddle.docs

import ascent.html.Html
import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Docs-as-tests site builder (Test classpath; `docs/specularSite`). */
object BuildSite extends DocsSite:

  @navLabel("Start")
  final case class Start(why: WhyHeddle.type, started: GettingStarted.type, hosts: ThreeHosts.type)

  @navLabel("Tutorial")
  final case class Tutorial(
      domain: Domain.type,
      operations: Operations.type,
      bind: BindEffect.type,
      job: TheToolIsTheJob.type,
      docsHttp: DocsAndHttp.type,
      agents: AgentsFallOut.type,
      web: WebApp.type,
  )

  @navLabel("Compiler")
  final case class Compiler(ast: TheAst.type, effects: Effects.type)

  @navLabel("HTTP")
  final case class HttpNav(http: Http.type, stays: WhatStaysOpen.type, endpoints: Endpoints.type)

  @navLabel("Agents")
  final case class AgentsNav(agents: Agents.type)

  @navLabel("Auth")
  final case class AuthNav(auth: Auth.type)

  @navLabel("Realtime")
  final case class RealtimeNav(realtime: Realtime.type)

  @navLabel("Reference")
  final case class ReferenceNav(reference: Reference.type)

  final case class HeddleNav(
      start: Start,
      tutorial: Tutorial,
      compiler: Compiler,
      http: HttpNav,
      agents: AgentsNav,
      auth: AuthNav,
      realtime: RealtimeNav,
      reference: ReferenceNav,
  ) derives SiteNav

  private val siteNav: NavModel = SiteNav[HeddleNav].toNavModel

  def pages: Vector[DocPage] = siteNav.pages

  override def site: SiteModel =
    val m       = meta
    val branded = EarlyEffectTheme.brand(super.site)
    branded.copy(
      nav = Some(siteNav),
      pages = siteNav.pages,
      clientScript = Some("assets/client.js"),
      summaryMarkdown = Some("Start at [The hub](the-hub.html)."),
      installSnippets = Vector.empty,
      brand = Some(
        Brand(
          name = m.title.getOrElse("heddle"),
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/heddle")),
        )
      ),
    )
  end site

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): Task[Unit] =
    val _ = result
    EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out) *> writeLanding(out)

  private def writeLanding(out: Path): Task[Unit] =
    Html.renderPage(Landing.document).flatMap { page =>
      ZIO.attempt {
        val html =
          s"""<!DOCTYPE html>
             |<html lang="en">
             |<head>
             |  <meta charset="utf-8"/>
             |  <meta name="viewport" content="width=device-width, initial-scale=1"/>
             |  <title>heddle</title>
             |  <meta name="description" content="Write the service once. HTTP, OpenAPI, and MCP are hosts of the same BoundOp."/>
             |  <link rel="icon" href="images/logo.png"/>
             |  <link rel="stylesheet" href="assets/theme.css"/>
             |  <link rel="stylesheet" href="assets/index.css"/>
             |  <script type="module" src="assets/client.js"></script>
             |  <style>html,body{margin:0;background:var(--specular-bg);}</style>
             |</head>
             |<body>
             |${page.html}
             |</body>
             |</html>
             |""".stripMargin
        Files.writeString(out.resolve("index.html"), html, StandardCharsets.UTF_8)
        Files.writeString(out.resolve("assets/index.css"), page.css, StandardCharsets.UTF_8)
        ()
      }
    }

  private def copyClientBundle(out: Path): Task[Unit] =
    ZIO.attempt {
      val dest = out.resolve("assets/client.js")
      val src  = findClientJs.getOrElse {
        throw new RuntimeException(
          "JS client not linked; run docs/specularSite (or docsJS/fastLinkJS) first. " +
            s"Looked for marker ${clientJsMarker}"
        )
      }
      Files.createDirectories(dest.getParent)
      Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
      ()
    }

  private def clientJsMarker: Path =
    repoRoot.resolve("target/specular-client-js.path")

  private def findClientJs: Option[Path] =
    val marker = clientJsMarker
    if !Files.isRegularFile(marker) then None
    else
      val line = Files.readString(marker).nn.trim
      if line.isEmpty then None
      else
        val path = Paths.get(line)
        Option.when(Files.isRegularFile(path))(path)

  private def repoRoot: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath.nn)(p => Option(p.getParent).orNull)
      .takeWhile(_ != null)
      .find(p => Files.exists(p.resolve("build.sbt")))
      .getOrElse(Paths.get("").toAbsolutePath.nn)
end BuildSite
