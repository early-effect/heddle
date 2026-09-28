import org.scalajs.linker.interface.ModuleKind
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.{NativeTags, nativeConfig}
import chekhov.ChekhovBrowser

// Debug-mode Native frames are large enough that ZIO's run loop overflows Native's 1 MB thread stack
// default on macOS. 8 MB matches the Linux default, so local and CI runs behave the same.
lazy val nativeThreads: Seq[sbt.Setting[?]] =
  Seq(
    nativeConfig ~= (_.withMultithreading(true)),
    Test / envVars += "SCALANATIVE_THREAD_STACK_SIZE" -> "8m",
  )

lazy val nativeOpenssl: Seq[sbt.Setting[?]] = {
  val brew           = new java.io.File("/opt/homebrew/opt/openssl@3")
  val (cflags, libs) =
    if (brew.isDirectory)
      (Seq(s"-I${brew.getPath}/include"), Seq(s"-L${brew.getPath}/lib", "-lssl", "-lcrypto"))
    else (Seq.empty[String], Seq("-lssl", "-lcrypto"))
  Seq(
    nativeConfig ~= { c =>
      c.withCompileOptions(c.compileOptions ++ cflags)
        .withLinkingOptions(c.linkingOptions ++ libs)
    }
  )
}

/** Sources the JVM and Native builds share: both have `java.nio` files and blocking reads. */
lazy val jvmNativeShared: Seq[sbt.Setting[?]] =
  Seq(
    Compile / unmanagedSourceDirectories += Def.uncached(
      projectMatrixBaseDirectory.value / "src" / "main" / "scalajvmnative"
    )
  )

MyVersions.settings
HeddleZipx.settings

val scala3Version: String = MyVersions.scala
val scalaVersions         = Seq(scala3Version)

Global / concurrentRestrictions ++= Seq(
  Tags.limit(NativeTags.Link, 1),
  Tags.limit(Tags.Compile, 4),
)

organization         := "rocks.earlyeffect"
organizationName     := "Early Effect"
organizationHomepage := Some(uri("https://www.earlyeffect.rocks"))
licenses             := List("Apache-2.0" -> uri("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage             := Some(uri("https://github.com/early-effect/heddle"))
scmInfo              := Some(
  ScmInfo(
    uri("https://github.com/early-effect/heddle"),
    "scm:git@github.com:early-effect/heddle.git",
  )
)
developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = uri("https://github.com/russwyte"),
  )
)
versionScheme := Some("early-semver")

publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
publishMavenStyle    := true
pomIncludeRepository := { _ => false }

// CI-only publishing: the signing key hex comes from the PGP_KEY_HEX env var, set by
// the shared early-effect org secret in the generated release job. There is no real key
// in this file: the "MISSING_KEY_HEX" sentinel keeps the build loadable for local
// compile/test but makes signing fail loudly if anyone tries to publish off-CI.
usePgpKeyHex(sys.env.getOrElse("PGP_KEY_HEX", "MISSING_KEY_HEX"))

lazy val exampleMcpStdioCp = taskKey[Unit]("write classpath for example MCP stdio subprocess")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Wunused:all",
    "-feature",
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
)

lazy val root = project
  .in(file("."))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .aggregate(
    (heddle.projectRefs ++ brotli.projectRefs ++ oauth.projectRefs ++ mcpProtocol.projectRefs ++ mcp.projectRefs ++
      mcpApps.projectRefs ++
      Seq[sbt.ProjectReference](example, bench, docs, docsJS, appsBrowser))*
  )
  .settings(
    name           := "heddle-root",
    publish / skip := true,
    test / skip    := true,
  )

lazy val heddle = (projectMatrix in file("heddle"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle",
    description          := "HTTP as an effect. Loom under the floor.",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )
  .jvmPlatform(scalaVersions = scalaVersions, jvmNativeShared)
  .jsPlatform(
    scalaVersions = scalaVersions,
    MyVersions.jsRuntime ++ Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    ),
  )
  .nativePlatform(
    scalaVersions = scalaVersions,
    MyVersions.nativeJavaTime ++ nativeThreads ++ nativeOpenssl ++ jvmNativeShared,
  )

lazy val brotli = (projectMatrix in file("brotli"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(heddle % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-brotli",
    description          := "RFC 7932 brotli encoder for heddle",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )
  .settings(MyVersions.brotliTest)
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(
    scalaVersions = scalaVersions,
    MyVersions.jsRuntime ++ Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    ),
  )
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime ++ nativeThreads ++ nativeOpenssl)

lazy val oauth = (projectMatrix in file("oauth"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(heddle % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-oauth",
    description          := "OAuth2 / OIDC client, resource server, and provider for heddle",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
    Compile / run / fork := true,
    Compile / mainClass  := Some("heddle.oauth.provider.ProviderApp"),
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(
    scalaVersions = scalaVersions,
    MyVersions.jsRuntime ++ Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    ),
  )

lazy val mcpProtocol = (projectMatrix in file("mcp-protocol"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-mcp-protocol",
    description          := "MCP wire types and codecs: JSON-RPC, tools, resources, capabilities",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions, MyVersions.jsRuntime)
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime ++ nativeThreads)

lazy val mcp = (projectMatrix in file("mcp"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(heddle % "compile->compile;test->test", mcpProtocol % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-mcp",
    description          := "MCP 2026-07-28 server for heddle endpoints",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(
    scalaVersions = scalaVersions,
    MyVersions.jsRuntime ++ Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    ),
  )
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime ++ nativeThreads ++ nativeOpenssl)

lazy val example = project
  .in(file("example"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(oauth.jvm(scala3Version), mcp.jvm(scala3Version))
  .settings(commonSettings)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-example",
    publish / skip       := true,
    Compile / run / fork := true,
    Test / fork          := true,
    exampleMcpStdioCp    := Def.uncached {
      val conv = fileConverter.value
      val cp   = (Test / fullClasspath).value
        .map(a => conv.toPath(a.data).toAbsolutePath.toString)
        .mkString(java.io.File.pathSeparator)
      val dest = java.nio.file.Path.of("example", "target", "mcp-stdio.classpath")
      java.nio.file.Files.createDirectories(dest.getParent)
      java.nio.file.Files.writeString(dest, cp)
      ()
    },
    Test / executeTests := Def.uncached {
      exampleMcpStdioCp.value
      (Test / executeTests).value
    },
  )

lazy val bench = project
  .in(file("bench"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(heddle.jvm(scala3Version))
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.benchLib)
  .settings(
    name                 := "heddle-bench",
    publish / skip       := true,
    test / skip          := true,
    Compile / run / fork := true,
  )

lazy val perfTests = project
  .in(file("perfTests"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(heddle.jvm(scala3Version) % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name           := "heddle-perf-tests",
    publish / skip := true,
  )

lazy val docsJS = project
  .in(file("docs-js"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name           := "heddle-docsJS",
    publish / skip := true,
    zipxPublish    := Some(false),
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    Compile / mainClass := Some("heddle.docs.ClientMain"),
    Compile / unmanagedSourceDirectories += (LocalProject("docs") / baseDirectory).value / "shared" / "scala",
    MyVersions.docsJs,
  )

lazy val mcpApps = (projectMatrix in file("mcp-apps"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(mcp % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-mcp-apps",
    description          := "MCP Apps (SEP-1865): sheds of typed grants, ui:// resources, and the policy a host clamps",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(
    scalaVersions = scalaVersions,
    MyVersions.jsRuntime ++ Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    ),
  )
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime ++ nativeThreads ++ nativeOpenssl)

lazy val browserCheck = taskKey[Unit]("fail if the browser-side MCP surface links a Node module")

/** Links what an MCP App view uses as a browser ES module. Heddle's JS target also serves Node, so a `node:` import
  * reachable from that surface would break every view; this keeps the split honest.
  */
lazy val browser = project
  .in(file("browser-check"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(heddle.js(scala3Version), mcp.js(scala3Version), mcpApps.js(scala3Version))
  .settings(commonSettings)
  .settings(
    name                            := "heddle-browser-check",
    publish / skip                  := true,
    zipxPublish                     := Some(false),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    browserCheck := Def.uncached {
      (Compile / fullLinkJS).value
      val out   = (Compile / fullLinkJSOutput).value
      val code  = IO.listFiles(out).filter(_.getName.endsWith(".js")).map(IO.read(_)).mkString("\n")
      val nodes = """(?:from|import)\s*\(?\s*["'](node:[^"']+|fs|net|tls|zlib|crypto|child_process|process)["']""".r
        .findAllMatchIn(code)
        .map(_.group(1))
        .toList
        .distinct
      if nodes.nonEmpty then sys.error(s"browser bundle imports Node modules: ${nodes.mkString(", ")}")
      streams.value.log.info(s"browser bundle is Node-free (${code.length / 1024} KiB)")
    },
  )

/** What the MCP Apps sandbox assumes of real browsers, proven in Chromium, Firefox, and WebKit with Chekhov. Test-only
  * and never published; it grows into the host kit's hostile-view suite.
  */
lazy val appsBrowser = project
  .in(file("apps-browser"))
  .dependsOn(heddle.jvm(scala3Version), mcpApps.jvm(scala3Version))
  .settings(commonSettings)
  .settings(
    name            := "heddle-apps-browser",
    publish / skip  := true,
    zipxPublish     := Some(false),
    MyVersions.coreTest,
    MyVersions.browserTest,
    chekhovBrowsers := Seq(ChekhovBrowser.Chromium, ChekhovBrowser.Firefox, ChekhovBrowser.WebKit),
    Test / fork     := true,
  )

lazy val docs = project
  .in(file("docs"))
  .disablePlugins(chekhov.sbt.ChekhovPlugin)
  .dependsOn(
    heddle.jvm(scala3Version),
    brotli.jvm(scala3Version),
    oauth.jvm(scala3Version),
    mcp.jvm(scala3Version),
    mcpApps.jvm(scala3Version),
  )
  .enablePlugins(SpecularPlugin)
  .settings(commonSettings)
  .settings(
    name            := "heddle-docs",
    publish / skip  := true,
    publishArtifact := false,
    zipxPublish     := Some(false),
    scalacOptions ++= Seq("-language:implicitConversions"),
    Test / unmanagedSourceDirectories += baseDirectory.value / "shared" / "scala",
    MyVersions.docsTest,
    MyVersions.coreTest,
    dependencyOverrides += MyVersions.moduleID(MyVersions.zioJson),
    libraryDependencySchemes += "rocks.earlyeffect" %% "heddle" % VersionScheme.Always,
    // ascent-preview 0.7.1 / specular 0.16.4 still pull published heddle 0.2.0. Docs
    // preview must run against this tree. Files is back on import heddle.* so the
    // preview bytecode (exports$package$.Files) links.
    excludeDependencies ++= Seq(
      "rocks.earlyeffect" %% "heddle",
      "rocks.earlyeffect" %% "heddle-mcp",
      "rocks.earlyeffect" %% "heddle-oauth",
    ),
    Test / mainClass       := None,
    specularBuildMain      := "heddle.docs.BuildSite",
    specularMetaProject    := Some(LocalProject("heddle")),
    specularArtifactKind   := "library",
    specularSiteDirectory  := (ThisBuild / baseDirectory).value / "target" / "site",
    specularDisplayVersion := {
      val fallback = previousStableVersion.value.getOrElse("0.2.0")
      (v: String) => {
        val stripped = stripCi(v)
        if (stripped != v) stripped
        else if (v.contains('+')) fallback
        else v
      }
    },
    specularJsLink := Def.uncached {
      (docsJS / Compile / fastLinkJS).value
      val outDir = (docsJS / Compile / fastLinkJSOutput).value
      val mainJs = outDir / "main.js"
      if (!mainJs.exists)
        sys.error(
          s"Expected $mainJs after fastLinkJS; directory contains: " +
            Option(outDir.list).toSeq.flatten.mkString(", ")
        )
      val dest = specularSiteDirectory.value / "assets" / "client.js"
      IO.createDirectory(dest.getParentFile)
      IO.copyFile(mainJs, dest)
      val marker = (ThisBuild / baseDirectory).value / "target" / "specular-client-js.path"
      IO.write(marker, mainJs.getAbsolutePath)
    },
    specularJsLinkDev := Def.uncached(specularJsLink.value),
  )

// Real browsers, kept out of testJVM so the everyday gates never launch one.
addCommandAlias("testBrowsers", "appsBrowser/testFull")
addCommandAlias("docsPreview", "~docs/specularPreview")
addCommandAlias(
  "testJVM",
  "heddle/testFull; brotli/testFull; oauth/testFull; mcpProtocol/testFull; mcp/testFull; mcpApps/testFull; example/testFull; docs/testFull; docs/specularSite",
)
addCommandAlias(
  "testJS",
  "heddleJS/testFull; brotliJS/testFull; mcpProtocolJS/testFull; mcpJS/testFull; mcpAppsJS/testFull; oauthJS/testFull; browser/browserCheck",
)
addCommandAlias(
  "testNative",
  "heddleNative/testFull; brotliNative/testFull; mcpProtocolNative/testFull; mcpNative/testFull; mcpAppsNative/testFull",
)
