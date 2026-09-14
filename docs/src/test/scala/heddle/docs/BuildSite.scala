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

  @navLabel("HTTP")
  final case class HttpNav(http: Http.type)

  @navLabel("Endpoints")
  final case class EndpointsNav(endpoints: Endpoints.type)

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
      http: HttpNav,
      endpoints: EndpointsNav,
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
    val v       = m.docsVersion
    val org     = m.organization
    branded.copy(
      nav = Some(siteNav),
      pages = siteNav.pages,
      clientScript = Some("assets/client.js"),
      summaryMarkdown = Some(
        """**Heddle** is HTTP as an effect. Write a capability once (`BoundOp` / `Api`) and host it
for humans, systems, and agents: HTTP + OpenAPI for browsers and services, MCP over Streamable HTTP
or stdio for agents.

Handlers are `Request => ZIO[R, E, Response]`. Routing is data. Middleware is `@@`. Loom runs
accept/read/write as ordinary ZIO. You can stop at `Routes` + `HeddleApp`. `Api` is how the other
hosts appear, not a rewrite.
"""
      ),
      installSnippets = Vector(
        CodeSnippet(
          "Core",
          s"""libraryDependencies += "$org" %% "heddle" % "$v"""",
        ),
        CodeSnippet(
          "JSON (zio-json)",
          s"""libraryDependencies += "$org" %% "heddle-zio-json" % "$v"""",
        ),
        CodeSnippet(
          "MCP (agents)",
          s"""libraryDependencies += "$org" %% "heddle-mcp" % "$v"""",
        ),
        CodeSnippet(
          "OAuth / OIDC",
          s"""libraryDependencies += "$org" %% "heddle-oauth" % "$v"""",
        ),
        CodeSnippet(
          "Brotli",
          s"""libraryDependencies += "$org" %% "heddle-brotli" % "$v"""",
        ),
      ),
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
    EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out)

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
