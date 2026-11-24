package heddle.build

import histos.*

object Catalog:
  val scala3           = scala"3.10.1"
  val zio              = dep"dev.zio::zio:2.1.27"
  val zioStreams       = zio.module("zio-streams")
  val zioTest          = zio.module("zio-test")
  val zioTestSbt       = zio.module("zio-test-sbt")
  val zioJson          = dep"dev.zio::zio-json:1.1.2"
  val neotype          = dep"io.github.kitlangton::neotype:0.7.2"
  val zioHttp          = dep"dev.zio::zio-http:3.11.6"
  val brotliDec        = dep"org.brotli:dec:0.1.2"
  val zioOpentelemetry = dep"dev.zio::zio-opentelemetry:3.1.21"
  val otelSdk          = dep"io.opentelemetry:opentelemetry-sdk:1.68.0"
  val otelSdkTesting   = dep"io.opentelemetry:opentelemetry-sdk-testing:1.68.0"
  val scalaJavaTime    = dep"io.github.cquiroz::scala-java-time:2.10.0"
  val tastyInspector   = dep"org.scala-lang::scala3-tasty-inspector:3.10.1"
  val specular         = dep"rocks.earlyeffect::specular-core:0.21.5"
  val specularZioTest  = specular.module("specular-zio-test")
  val specularTheme    = specular.module("early-effect-docs-theme")
  val mermoidAscent    = dep"rocks.earlyeffect::mermoid-ascent:0.2.0"
  val ascentDomFacade  = dep"rocks.earlyeffect::ascent-dom-facade:0.11.2"
  val ascentDomTypes   = dep"rocks.earlyeffect::ascent-dom-types:0.11.2"
  val ascentCore       = dep"rocks.earlyeffect::ascent-core:0.10.2"
  val ascentJs         = dep"rocks.earlyeffect::ascent-js:0.11.1"
  val ascentCss        = dep"rocks.earlyeffect::ascent-css:0.10.2"
  val chekhov          = dep"rocks.earlyeffect::chekhov-zio-test:0.1.3"
  val chekhovDriver    = chekhov.module("chekhov-driver")
  val slayer           = dep"rocks.earlyeffect::slayer:0.2.0"

  def flagged[P](module: ModuleDef[P]): ModuleDef[P] =
    module.scalac(ScalacFlag.Deprecation, ScalacFlag.UnusedAll, ScalacFlag.Feature)

  def tested[P](module: ModuleDef[P]): ModuleDef[P] =
    flagged(module).testDependencies(zioTest, zioTestSbt)

  def core[P](module: ModuleDef[P]): ModuleDef[P] =
    tested(module).libraryDependencies(zio, zioStreams, zioJson)
end Catalog
