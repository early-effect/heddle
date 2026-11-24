package heddle.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, StandardCopyOption}

/** Validates source configuration before the site evaluates examples. */
object BuildSite extends ZIOAppDefault:
  def pages: Vector[DocPage] = HeddleSite.pages

  private[docs] def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] = HeddleSite.afterBuild(out, result)

  def build: IO[SiteError, SiteOutput] =
    for
      root   <- RepoRoot.load.mapError(error => SiteError.Settings(Config.Error.InvalidData(message = error.toString)))
      result <- ZIO.configProviderWith: provider =>
        val sources = ConfigProvider.fromMap(Map("specular.source.root" -> root.path.toString))
        ZIO.withConfigProvider(sources.orElse(provider))(HeddleSite.build)
    yield result

  def run = build.foldZIO(
    error => Console.printLineError(s"specular: ${error.message}").ignore.as(ExitCode.failure),
    result => Console.printLine(s"Wrote ${result.pages.mkString(", ")}").ignore.as(ExitCode.success),
  )
end BuildSite

/** Docs-as-tests site builder on the docs JVM Test classpath. */
private object HeddleSite extends DocsSite:

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
  final case class HttpNav(
      http: Http.type,
      stays: WhatStaysOpen.type,
      endpoints: Endpoints.type,
      observability: Observability.type,
  )

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
    val client = out.resolve("assets/client.js")
    for
      exists <- ZIO.attemptBlockingIO(Files.isRegularFile(client)).mapError(SiteError.FileUnreadable(client, _))
      _      <- ZIO.fail(SiteError.MissingFile(client)).unless(exists)
      _      <- promote
      _      <- EarlyEffectTheme.writeLogo(out)
    yield ()
end HeddleSite
