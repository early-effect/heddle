import zipx.*

/** Typed catalog. `zipxDepUpdate` rewrites constructors here. sbt-zipx and sbt-pgp are not rows. */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.1.0-M3")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val zio        = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams = zio.mod("zio-streams")
  val zioTest    = zio.mod("zio-test").test
  val zioTestSbt = zio.mod("zio-test-sbt").test
  val zioJson    = Lib("dev.zio", "zio-json", "1.1.0")
  val zioHttp    = Lib("dev.zio", "zio-http", "3.11.6")
  val brotliDec  = Lib("org.brotli", "dec", "0.1.2").java.test

  val scalaJavaTime = Lib("io.github.cquiroz", "scala-java-time", "2.7.0")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.19.0-7251800ecbdc-SNAPSHOT")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test
  val ascentDomFacade = Lib("rocks.earlyeffect", "ascent-dom-facade", "0.11.0")
  // Not direct dependencies. Stated so these releases overrule the commit pins ascent-js and specular still bring.
  val ascentDomTypes  = Lib("rocks.earlyeffect", "ascent-dom-types", "0.11.0")
  val ascentCore      = Lib("rocks.earlyeffect", "ascent-core", "0.10.0")
  val ascentJs        = Lib("rocks.earlyeffect", "ascent-js", "0.11.0-19667f3cf23f-SNAPSHOT")
  val ascentCss       = Lib("rocks.earlyeffect", "ascent-css", "0.10.0")
  val chekhov         = Lib("rocks.earlyeffect", "chekhov-zio-test", "0.1.3").test
  val chekhovDriver   = chekhov.mod("chekhov-driver").test

  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val scalajs        = Plugin("org.scala-js", "sbt-scalajs", "1.22.0")
  val scalaNative    = Plugin("org.scala-native", "sbt-scala-native", "0.5.12")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.19.0-7251800ecbdc-SNAPSHOT")
  val chekhovPlugin  = Plugin("rocks.earlyeffect", "sbt-chekhov", "0.1.3")
  val splicePlugin   = Plugin("rocks.earlyeffect", "sbt-splice", "0.3.2-8464547dc109-SNAPSHOT")

  val release = ShipGroup("heddle", "0.9.0")(
    "heddle",
    "brotli",
    "oauth",
    "mcpProtocol",
    "mcp",
    "mcpApps",
    "mcpAppsHost",
    "mcpAppsFrame",
  )

  def coreLib     = library(zio, zioStreams, zioJson)
  def coreTest    = library(zioTest, zioTestSbt)
  def benchLib    = library(zioHttp)
  def brotliTest  = library(brotliDec)
  def docsTest    = library(specularZioTest, specularTheme, ascentCss)
  def docsJs      = library(specular, ascentJs, ascentCss, zio)
  def browserTest = library(chekhov, chekhovDriver)

  /** The browser DOM, as ascent's WebIDL-generated facade types it: what an MCP App's relay, frame, and view touch. */
  def domFacade = library(ascentDomFacade)

  /** Heddle reads only `Instant` and UTC offsets, so region time zones (tzdb) stay the application's choice. */
  def javaTime  = library(scalaJavaTime)
  def jsRuntime = javaTime
end MyVersions
