import sbt.{Def, Setting}
import sbt.Keys.{dependencyOverrides, libraryDependencySchemes}
import sbt.librarymanagement.syntax.*
import zipx.*

/** Typed catalog. `zipxDepUpdate` rewrites constructors here. sbt-zipx and sbt-pgp are not rows. */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.1.0-M1")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val zio        = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams = zio.mod("zio-streams")
  val zioTest    = zio.mod("zio-test").test
  val zioTestSbt = zio.mod("zio-test-sbt").test
  val zioJson    = Lib("dev.zio", "zio-json", "1.1.0")
  val zioHttp    = Lib("dev.zio", "zio-http", "3.11.6")
  val nimbusJose = Lib("com.nimbusds", "nimbus-jose-jwt", "10.9.1").java
  val nimbusOidc = Lib("com.nimbusds", "oauth2-oidc-sdk", "11.28").java
  val brotliDec  = Lib("org.brotli", "dec", "0.1.2").java.test

  val scalaJavaTime     = Lib("io.github.cquiroz", "scala-java-time", "2.7.0")
  val scalaJavaTimeTzdb = scalaJavaTime.mod("scala-java-time-tzdb")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.17.0")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test
  val ascentJs        = Lib("rocks.earlyeffect", "ascent-js", "0.7.1")
  val ascentCss       = ascentJs.mod("ascent-css")

  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val dynver         = Plugin("com.github.sbt", "sbt-dynver", "5.1.1")
  val scalajs        = Plugin("org.scala-js", "sbt-scalajs", "1.22.0")
  val scalaNative    = Plugin("org.scala-native", "sbt-scala-native", "0.5.12")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.17.0")

  def coreLib    = library(zio, zioStreams, zioJson)
  def coreTest   = library(zioTest, zioTestSbt)
  def benchLib   = library(zioHttp)
  def oauthLib   = library(nimbusJose, nimbusOidc)
  def brotliTest = library(brotliDec)
  def docsTest   = library(specularZioTest, specularTheme, ascentCss)
  def docsJs     = library(specular, ascentJs, ascentCss, zio)
  def javaTime  = library(scalaJavaTime, scalaJavaTimeTzdb)
  def jsRuntime = javaTime

  def nativeTestInterface: Seq[Setting[?]] =
    val testInterface = "org.scala-native" % "test-interface_native0.5_3" % (scalaNative.version: String)
    Seq(
      libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % "early-semver",
      dependencyOverrides += Def.uncached(testInterface),
    )

  def nativeJavaTime = javaTime ++ nativeTestInterface
end MyVersions
