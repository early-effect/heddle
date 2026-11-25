package heddle.docs

import ascent.{Eq, sq}
import ascent.html.Html
import heddle.docs.ui.Hub
import specular.site.{SiteError, SiteOutput}
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object BuildSiteSpec extends ZIOSpecDefault:
  private enum Gap:
    case NoFrontPage

  def spec = suite("staged docs site")(
    test("front promotion and logo writing retain the adapter's client and metadata") {
      ZIO.scoped:
        for
          stage <- temporary
          front <- ZIO.fromOption(BuildSite.pages.headOption).orElseFail(Gap.NoFrontPage)
          _     <- ZIO.attemptBlockingIO {
            Files.createDirectories(stage.resolve("assets"))
            Files.writeString(stage.resolve(s"${front.slug}.html"), "front page")
            Files.writeString(stage.resolve("index.html"), "summary")
            Files.writeString(stage.resolve("assets/client.js"), "linked widgets")
            Files.writeString(stage.resolve("metadata.json"), "released metadata")
          }
          _        <- BuildSite.afterBuild(stage, SiteOutput(stage, Vector.empty))
          index    <- ZIO.attemptBlockingIO(Files.readString(stage.resolve("index.html")))
          client   <- ZIO.attemptBlockingIO(Files.readString(stage.resolve("assets/client.js")))
          metadata <- ZIO.attemptBlockingIO(Files.readString(stage.resolve("metadata.json")))
          logo <- ZIO.attemptBlockingIO(Files.isRegularFile(stage.resolve(earlyeffect.docs.EarlyEffectTheme.logoHref)))
        yield assertTrue(index == "front page", client == "linked widgets", metadata == "released metadata", logo)
    },
    test("startup rejects missing source configuration before building the site") {
      for error <- ZIO.withConfigProvider(ConfigProvider.fromMap(Map.empty))(BuildSite.build).flip
      yield assertTrue(error match
        case SiteError.Settings(_) => true
        case _                     => false)
    },
    test("a missing staged client fails with its actual path, not an sbt marker") {
      ZIO.scoped:
        for
          stage <- temporary
          error <- BuildSite.afterBuild(stage, SiteOutput(stage, Vector.empty)).flip
        yield assertTrue(error == SiteError.MissingFile(stage.resolve("assets/client.js")))
    },
    test("shared choice widgets use their declared equality for the selected class") {
      final case class Choice(key: Int, label: String)
      given Eq[Choice] = (left, right) => left.key == right.key
      for
        current <- sq(Choice(1, "current"))
        ui = Hub.choiceButton(current, Choice(1, "other label"), "Choice")
        selected   <- Html.render(ui)
        _          <- current.set(Choice(2, "different"))
        deselected <- Html.render(ui)
      yield assertTrue(
        selected.contains(s"class=\"${Hub.BtnOn.className}\""),
        deselected.contains(s"class=\"${Hub.Btn.className}\""),
      )
    },
    test("installation examples use the current release rather than the open ship line") {
      assertTrue(
        DocsVersion.published == "0.15.2",
        DocsVersion.version == "0.15.2",
        DocsVersion.advertise("0.16.0-SNAPSHOT") == "0.15.2",
      )
    },
  )

  private def temporary: ZIO[Scope, java.io.IOException, Path] =
    ZIO.acquireRelease(ZIO.attemptBlockingIO(Files.createTempDirectory("heddle-site-stage-")))(path =>
      ZIO.attemptBlockingIO {
        val files = Files.walk(path)
        try files.iterator.asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
        finally files.close()
      }.ignore
    )
end BuildSiteSpec
