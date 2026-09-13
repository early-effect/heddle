MyVersions.settings

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

zipxJavaVersion      := JdkVersion("25")
zipxWorkflowDispatch := true
zipxEnv              := Map(
  "JAVA_OPTS" -> EnvValue.plain(
    "-Xms2048M -Xmx2048M -Xss6M -XX:ReservedCodeCacheSize=256M -Dfile.encoding=UTF-8"
  )
)
zipxCapabilities ++= {
  val upstream = JobCondition.repositoryIs("early-effect/heddle")
  Seq(ZipxCentral.release.withCondition(upstream))
}

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
  .aggregate(heddle, json, brotli, oauth, example)
  .settings(
    name           := "heddle-root",
    publish / skip := true,
    test / skip    := true,
  )

lazy val heddle = project
  .in(file("heddle"))
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name                 := "heddle",
    description          := "HTTP as an effect. Loom under the floor.",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )

lazy val brotli = project
  .in(file("brotli"))
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

lazy val json = project
  .in(file("json"))
  .dependsOn(heddle % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.jsonLib)
  .settings(MyVersions.jsonTest)
  .settings(
    name                 := "heddle-zio-json",
    description          := "zio-json JsonCodec for heddle",
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
  )

lazy val oauth = project
  .in(file("oauth"))
  .dependsOn(heddle % "compile->compile;test->test", json)
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

lazy val example = project
  .in(file("example"))
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name                 := "heddle-example",
    publish / skip       := true,
    test / skip          := true,
    Compile / run / fork := true,
  )

lazy val bench = project
  .in(file("bench"))
  .dependsOn(heddle)
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
  .dependsOn(heddle % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name           := "heddle-perf-tests",
    publish / skip := true,
  )
