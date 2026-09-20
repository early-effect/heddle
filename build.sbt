import org.scalajs.linker.interface.ModuleKind
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.NativeTags

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
organizationHomepage := Some(url("https://www.earlyeffect.rocks"))
licenses             := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage             := Some(url("https://github.com/early-effect/heddle"))
scmInfo              := Some(
  ScmInfo(
    url("https://github.com/early-effect/heddle"),
    "scm:git@github.com:early-effect/heddle.git",
  )
)
developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = url("https://github.com/russwyte"),
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
  .aggregate(
    (heddle.projectRefs ++ brotli.projectRefs ++ oauth.projectRefs ++ mcp.projectRefs ++
      Seq[sbt.ProjectReference](example, bench, docs, docsJS))*
  )
  .settings(
    name           := "heddle-root",
    publish / skip := true,
    test / skip    := true,
  )

lazy val heddle = (projectMatrix in file("heddle"))
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle",
    description          := "HTTP as an effect. Loom under the floor.",
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
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime)

lazy val brotli = (projectMatrix in file("brotli"))
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
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime)

lazy val oauth = (projectMatrix in file("oauth"))
  .dependsOn(heddle % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(MyVersions.oauthLib)
  .settings(
    name                 := "heddle-oauth",
    description          := "OAuth2 / OIDC client, resource server, and provider for heddle",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
    Compile / run / fork := true,
    Compile / mainClass  := Some("heddle.oauth.provider.ProviderApp"),
  )
  .jvmPlatform(scalaVersions = scalaVersions)

lazy val mcp = (projectMatrix in file("mcp"))
  .dependsOn(heddle % "compile->compile;test->test")
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
  .nativePlatform(scalaVersions = scalaVersions, MyVersions.nativeJavaTime)

lazy val example = project
  .in(file("example"))
  .dependsOn(oauth.jvm(scala3Version), mcp.jvm(scala3Version))
  .settings(commonSettings)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle-example",
    publish / skip       := true,
    Compile / run / fork := true,
    Test / fork          := true,
    exampleMcpStdioCp := Def.uncached {
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

lazy val docs = project
  .in(file("docs"))
  .dependsOn(heddle.jvm(scala3Version), brotli.jvm(scala3Version), oauth.jvm(scala3Version), mcp.jvm(scala3Version))
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
    Test / mainClass := None,
    specularBuildMain      := "heddle.docs.BuildSite",
    specularMetaProject    := Some(LocalProject("heddle")),
    specularArtifactKind   := "library",
    specularSiteDirectory  := (ThisBuild / baseDirectory).value / "target" / "site",
    specularDisplayVersion := {
      val fallback = previousStableVersion.value.getOrElse("0.2.0")
      (v: String) =>
        val stripped = stripCi(v)
        if stripped != v then stripped
        else if v.contains('+') then fallback
        else v
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

addCommandAlias("docsPreview", "~docs/specularPreview")
addCommandAlias(
  "testJVM",
  "heddle/testFull; brotli/testFull; oauth/testFull; mcp/testFull; example/testFull; docs/testFull; docs/specularSite",
)
addCommandAlias("testJS", "heddleJS/testFull; brotliJS/testFull; mcpJS/testFull")
addCommandAlias("testNative", "heddleNative/testFull; brotliNative/testFull; mcpNative/testFull")
