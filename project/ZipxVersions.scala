import zipx.*

/** Typed catalog. `zipxDepUpdate` rewrites constructors here. sbt-zipx and sbt-pgp are not rows. */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.0.8")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val zio        = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams = zio.mod("zio-streams")
  val zioTest    = zio.mod("zio-test").test
  val zioTestSbt = zio.mod("zio-test-sbt").test
  val zioJson    = Lib("dev.zio", "zio-json", "1.1.0")
  val zioHttp    = Lib("dev.zio", "zio-http", "3.3.2")

  val scalafmt = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val dynver   = Plugin("com.github.sbt", "sbt-dynver", "5.1.1")

  def coreLib  = library(zio, zioStreams, zioJson)
  def coreTest = library(zioTest, zioTestSbt)
  def jsonLib  = library(zioJson)
  def jsonTest = library(zioTest, zioTestSbt)
  def benchLib = library(zioHttp)
end MyVersions
