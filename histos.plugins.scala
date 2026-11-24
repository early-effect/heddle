import histos.*
import histos.plugin.*

object plugins extends HistosPlugins:
  val buildTooling = module("buildTooling", Jvm)(scala"3.10.1")
    .in(path"build-tooling")
    .libraryDependencies(dep"dev.zio::zio:2.1.27")
    .testDependencies(dep"dev.zio::zio-test:2.1.27", dep"dev.zio::zio-test-sbt:2.1.27")
    .scalac(ScalacFlag.Deprecation, ScalacFlag.UnusedAll, ScalacFlag.Feature)

  override def resolvers: PluginResolvers = PluginResolvers.Listed(
    List(Resolver.Maven(RepositoryUrl("https://central.sonatype.com/repository/maven-snapshots/")), Resolver.Central)
  )

  def sources: PluginCatalog = PluginCatalog(
    PluginSources.module(buildTooling),
    PluginSources.ship(dep"rocks.earlyeffect::specular-histos:0.0.3-SNAPSHOT").link(LinkStrategy.BinaryOrSource),
  )
end plugins
