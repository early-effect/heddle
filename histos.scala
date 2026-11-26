import dev.earlyeffect.specular.{Site, SiteCi, Specular}
import heddle.build.*
import heddle.build.Catalog.*
import histos.*
import histos.plugin.{
  BuildServices,
  JobName,
  NoteKey,
  NoteText,
  PluginId,
  PluginTaskId,
  RepoPath,
  SitePublish,
  SourceSet,
  TaskName,
}
import _root_.zio.ZLayer

object build extends HistosBuild:
  private val playwright = ToolRevision("1.62.1")

  def graph: Either[BuildError, ProjectGraph] =
    val nativeNightly = NativeVersion("0.5.13-20261122-fa4437d-SNAPSHOT")
    val ship          = Ship("heddle")
    val heddle        = core(ship.parent(Jvm, Js, Native)(scala3))
      .description(pom"HTTP as an effect. Loom under the floor.")
      .libraryDependencies(neotype, scalaJavaTime.on(Js, Native))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
      .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
    val brotli = core(module("brotli", Jvm, Js, Native)(scala3))
      .description(pom"RFC 7932 brotli encoder for heddle")
      .dependsOn(heddle)
      .testDependsOn(heddle)
      .libraryDependencies(scalaJavaTime.on(Js, Native))
      .testDependencies(brotliDec.on(Jvm))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
      .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
    val oauth = core(module("oauth", Jvm, Js)(scala3))
      .description(pom"OAuth2 / OIDC client, resource server, and provider for heddle")
      .dependsOn(heddle)
      .testDependsOn(heddle)
      .libraryDependencies(scalaJavaTime.on(Js))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
      .main(MainClass("heddle.oauth.provider.ProviderApp"))
      .runPlatform(Jvm)
    val mcpProtocol = core(module("mcpProtocol", Jvm, Js, Native)(scala3))
      .in(path"mcp-protocol")
      .artifact(artifact"heddle-mcp-protocol")
      .description(pom"MCP wire types and codecs: JSON-RPC, tools, resources, capabilities")
      .libraryDependencies(scalaJavaTime.on(Js, Native))
      .jsVersion(JsVersion("1.23.0"))
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
    val mcp = core(module("mcp", Jvm, Js, Native)(scala3))
      .description(pom"MCP 2026-07-28 server for heddle endpoints")
      .dependsOn(heddle, mcpProtocol)
      .testDependsOn(heddle, mcpProtocol)
      .libraryDependencies(scalaJavaTime.on(Js, Native))
      .testDependencies(slayer.on(Jvm))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
      .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
    val mcpApps = tested(module("mcpApps", Jvm, Js, Native)(scala3))
      .in(path"mcp-apps")
      .artifact(artifact"heddle-mcp-apps")
      .description(pom"MCP Apps (SEP-1865): sheds of typed grants, ui:// resources, and the policy a host clamps")
      .dependsOn(mcp)
      .testDependsOn(mcp)
      .libraryDependencies(scalaJavaTime.on(Js, Native), ascentDomFacade.on(Js))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
      .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
    val mcpAppsRelay = flagged(module("mcpAppsRelay", Js)(scala3))
      .in(path"mcp-apps-relay")
      .dependsOn(mcpApps)
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.NoModule)
      .main(MainClass("heddle.mcp.apps.relay.Relay"))
    val mcpAppsHost = tested(module("mcpAppsHost", Jvm, Js)(scala3))
      .in(path"mcp-apps-host")
      .artifact(artifact"heddle-mcp-apps-host")
      .description(pom"MCP Apps host kit: effective policy, CSP, hash pins, audit, consent, and the view pipeline")
      .dependsOn(mcpApps)
      .testDependsOn(mcpApps)
      .libraryDependencies(scalaJavaTime.on(Js))
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.CommonJS)
    val mcpAppsFrame = flagged(module("mcpAppsFrame", Js)(scala3))
      .in(path"mcp-apps-frame")
      .artifact(artifact"heddle-mcp-apps-frame")
      .description(pom"MCP Apps host frame: the relay iframe and its handshake, in the host page")
      .dependsOn(mcpAppsHost)
      .jsVersion(JsVersion("1.23.0"))
    val opentelemetry = core(module("opentelemetry", Jvm)(scala3))
      .description(pom"OpenTelemetry spans for heddle servers and clients")
      .dependsOn(heddle)
      .testDependsOn(heddle)
      .libraryDependencies(zioOpentelemetry)
      .testDependencies(otelSdk, otelSdkTesting)
    val example = tested(module("example", Jvm)(scala3))
      .dependsOn(oauth, mcp)
      .main(MainClass("example.Main"))
    val bench = flagged(module("bench", Jvm)(scala3))
      .dependsOn(heddle)
      .libraryDependencies(zio, zioStreams, zioJson, zioHttp)
      .main(MainClass("bench.HeddlePlain"))
      .jvmOptions(
        JvmOptions(List(JvmFlag("-XX:+UnlockDiagnosticVMOptions"), JvmFlag("-XX:+DebugNonSafepoints")))
      )
    val perfTests = core(module("perfTests", Jvm)(scala3))
      .dependsOn(heddle)
      .testDependsOn(heddle)
      .lane(TestLane.Verify)
    val browser = flagged(module("browser", Js)(scala3))
      .in(path"browser-check")
      .dependsOn(heddle, mcp, mcpApps)
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.ESModule)
      .main(MainClass("heddle.browsercheck.Main"))
    val appsBrowserShared = flagged(module("appsBrowserShared", Jvm, Js)(scala3))
      .in(path"apps-browser/shared")
      .dependsOn(mcpAppsHost)
      .jsVersion(JsVersion("1.23.0"))
    val appsBrowserView = flagged(module("appsBrowserView", Js)(scala3))
      .in(path"apps-browser/view")
      .dependsOn(appsBrowserShared)
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.NoModule)
      .main(MainClass("heddle.apps.browser.view.CounterView"))
    val appsBrowserHost = flagged(module("appsBrowserHost", Js)(scala3))
      .in(path"apps-browser/host")
      .dependsOn(mcpAppsFrame, appsBrowserShared)
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.NoModule)
      .main(MainClass("heddle.apps.browser.host.ReferenceHost"))
    val appsBrowser = tested(module("appsBrowser", Jvm)(scala3))
      .in(path"apps-browser")
      .dependsOn(heddle, mcpAppsHost, appsBrowserShared)
      .testDependencies(chekhov, chekhovDriver)
      .lane(TestLane.Verify)
      .testProvider(TestProvider.Zio.properties(SystemProperty("chekhov.browsers=chromium,firefox,webkit")))
    val nativeTests = tested(module("nativeTests", Native)(scala3))
      .in(path"native-tests")
      .nativeVersion(nativeNightly)
      .clang(ClangMajor(21))
      .threadStack(ThreadStack("8m"))
      .link(NativeLibrary.Ssl, NativeLibrary.Crypto)
      .testAggregate(heddle, brotli, mcpProtocol, mcp, mcpApps)
    val library = ship.next(0, 16, 0)(
      heddle,
      brotli,
      oauth,
      mcpProtocol,
      mcp,
      mcpApps,
      mcpAppsHost,
      mcpAppsFrame,
      opentelemetry,
    )
    // Exact roots keep the JVM producer out of JS Main without duplicating the shared illustrations.
    val docs = tested(module("docs", Jvm, Js)(scala3))
      .in(path"docs/platform")
      .dependsOn(Jvm)(heddle, brotli, oauth, mcp, mcpApps, opentelemetry)
      .roots(
        ScopedRoot(RepoPath("docs/src/main/scala"), SourceSet.Main, PlatformSelection.only(Jvm), RootKind.Sources),
        ScopedRoot(RepoPath("docs/src/test/scala"), SourceSet.Test, PlatformSelection.only(Jvm), RootKind.Sources),
        ScopedRoot(RepoPath("docs/shared/scala"), SourceSet.Test, PlatformSelection.only(Jvm), RootKind.Sources),
        ScopedRoot(RepoPath("docs/shared/scala"), SourceSet.Main, PlatformSelection.only(Js), RootKind.Sources),
        ScopedRoot(RepoPath("docs-js/src/main/scala"), SourceSet.Main, PlatformSelection.only(Js), RootKind.Sources),
      )
      .scalac(ScalacFlag.Deprecation, ScalacFlag.UnusedAll, ScalacFlag.Feature, ScalacFlag.ImplicitConversions)
      .libraryDependencies(specular.on(Js), mermoidAscent.on(Js), ascentJs.on(Js), ascentCss, zio)
      .testDependencies(
        tastyInspector.on(Jvm),
        specularZioTest.on(Jvm),
        specularTheme.on(Jvm),
        mermoidAscent.on(Jvm),
        otelSdk.on(Jvm),
        otelSdkTesting.on(Jvm),
      )
      .jsVersion(JsVersion("1.23.0"))
      .jsModule(JsModuleKind.ESModule)
      .main(MainClass("heddle.docs.ClientMain"))
      .runPlatform(Js)
      .note(PluginId("dev.earlyeffect.specular"), NoteKey("library"), NoteText("0.16.0"))
    val buildTooling = tested(module("buildTooling", Jvm)(scala3))
      .in(path"build-tooling")
      .libraryDependencies(zio)
    Projects
      .organization(organization"rocks.earlyeffect")
      .plugins(
        PluginSources.module(buildTooling),
        PluginSources.ship(dep"rocks.earlyeffect::specular-histos:0.1.0-SNAPSHOT"),
      )
      .organizationName(pom"Early Effect")
      .organizationHomepage(MetadataUri("https://www.earlyeffect.rocks"))
      .versionScheme(VersionScheme.EarlySemver)
      .license(License(license"Apache-2.0", MetadataUri("http://www.apache.org/licenses/LICENSE-2.0.txt")))
      .developer(
        Developer(
          developer"russwyte",
          pom"Russ White",
          histos.supply.Email("356303+russwyte@users.noreply.github.com"),
          MetadataUri("https://github.com/russwyte"),
        )
      )
      .scm(
        Scm(
          MetadataUri("https://github.com/early-effect/heddle"),
          connection"scm:git@github.com:early-effect/heddle.git",
        )
      )
      .description(pom"HTTP as an effect. Loom under the floor.")
      .url(MetadataUri("https://github.com/early-effect/heddle"))
      .runnerTools(
        RunnerTools(
          Some(NodeFloor(25)),
          Some(ToolRevision("21.1.8")),
          Some(playwright),
          browsers = Set(Browser.Chromium, Browser.Firefox, Browser.WebKit),
          jdk = Some(JdkFloor(25)),
        )
      )
      .scalafmt
      .docCheck
      .verify(VerifyTask(JobName("js"), VerifyRun.Tests(TestSelection(platforms = PlatformSelection.only(Js)))))
      .verify(VerifyTask(JobName("native"), VerifyRun.Tests(TestSelection(platforms = PlatformSelection.only(Native)))))
      .verify(
        VerifyTask(
          JobName("browsers"),
          VerifyRun.Tests(
            TestSelection(TestTargets.Named(ProjectName("appsBrowser"), Nil), PlatformSelection.only(Jvm))
          ),
        )
      )
      .verify(
        VerifyTask(
          JobName("browser-check"),
          VerifyRun.Plugin(PluginTaskId(TaskSpec.pluginId, TaskName("browser-check"))),
        )
      )
      .resolvers(
        Resolver.Maven(RepositoryUrl("https://central.sonatype.com/repository/maven-snapshots/")),
        Resolver.Central,
      )(
        ships(library),
        heddle,
        brotli,
        oauth,
        mcpProtocol,
        mcp,
        mcpApps,
        mcpAppsHost,
        mcpAppsFrame,
        opentelemetry,
        example,
        bench,
        perfTests,
        mcpAppsRelay,
        browser,
        appsBrowserShared,
        appsBrowserView,
        appsBrowserHost,
        appsBrowser,
        nativeTests,
        docs,
        buildTooling,
      )

  override def services: BuildServices =
    BuildServices.default
      .add[RelayBrowserTasks](RelayBrowserLive.layer)
      .add[DocsTranscriptTask](TranscriptLive.layer)
      .tasks[HeddleBuildPlugin](HeddleBuildPlugin.layer)
      .add[Specular](ZLayer.succeed(site) >>> Specular.layer)
      .tasks[Specular](ZLayer.succeed(site) >>> Specular.layer)

  private val site = Site
    .forProject("docs", "heddle.docs.BuildSite")
    .basePath("/heddle")
    .docsUrl("https://early-effect.github.io/heddle/")
    .name("heddle")
    .organization("rocks.earlyeffect")
    .title("Heddle")
    .description("HTTP as an effect. Loom under the floor.")
    .homepage("https://github.com/early-effect/heddle")
    .libraryShip
    .version("0.15.2")
    .displayVersion("0.15.2")
    .linkClient
    .preview
    .ci(SiteCi.Publish(SitePublish.Both))
end build
