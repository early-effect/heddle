package heddle.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.*

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
  final case class AgentsNav(agents: Agents.type, apps: McpAppsPage.type)

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

  /** Not a nav item. The sidebar is the table of contents; this page is the front. */
  private val frontPage: DocPage = Front.doc

  def pages: Vector[DocPage] = frontPage +: siteNav.pages

  override def site(settings: DocsSettings): SiteModel =
    val branded = EarlyEffectTheme.brand(super.site(settings))
    branded.copy(
      nav = Some(siteNav),
      pages = pages,
      clientScript = Some("assets/client.js"),
      summaryMarkdown = None,
      installSnippets = Vector.empty,
      brand = Some(
        Brand(
          name = settings.meta.displayTitle,
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/heddle")),
        )
      ),
    )
  end site

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    // Specular always writes index.html as a summary plus a second copy of the nav.
    // The front DocPage is the index. Copy it over that file. Asset paths stay site-relative.
    val front   = out.resolve(s"${frontPage.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out)
  end afterBuild

  private def copyClientBundle(out: Path): IO[SiteError, Unit] =
    val dest = out.resolve("assets/client.js")
    findClientJs match
      case None =>
        ZIO.fail(SiteError.MissingFile(clientJsMarker))
      case Some(src) =>
        ZIO
          .attemptBlockingIO {
            Files.createDirectories(dest.getParent)
            Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
            ()
          }
          .mapError(err => SiteError.WriteFailed(dest, err))
    end match
  end copyClientBundle

  private def clientJsMarker: Path =
    repoRoot.resolve("target/specular-client-js.path")

  private def findClientJs: Option[Path] =
    val marker = clientJsMarker
    if !Files.isRegularFile(marker) then None
    else
      val line = Files.readString(marker).trim
      if line.isEmpty then None
      else
        val path = Paths.get(line)
        Option.when(Files.isRegularFile(path))(path)

  private def repoRoot: Path =
    val here = Paths.get("").toAbsolutePath
    Iterator
      .unfold(Option(here))(_.map(p => (p, Option(p.getParent))))
      .find(p => Files.exists(p.resolve("build.sbt")))
      .getOrElse(here)
end BuildSite
