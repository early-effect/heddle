package heddle.docs

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

object RepoRootSpec extends ZIOSpecDefault:
  def spec = suite("docs repository configuration")(
    test("the runner source root wins over the direct invocation fallback") {
      ZIO.scoped:
        for
          path <- repository
          root <- ZIO.withConfigProvider(
            ConfigProvider.fromMap(
              Map(
                "specular.source.root" -> path.toString,
                "HISTOS_REPO"          -> path.resolve("missing").toString,
              )
            )
          )(RepoRoot.load)
        yield assertTrue(root.path == path)
    },
    test("direct tests can provide HISTOS_REPO without changing JVM properties") {
      ZIO.scoped:
        for
          path <- repository
          root <- ZIO.withConfigProvider(ConfigProvider.fromMap(Map("HISTOS_REPO" -> path.toString)))(RepoRoot.load)
        yield assertTrue(root.path == path)
    },
    test("missing and malformed roots fail in the typed configuration channel") {
      for
        absent    <- ZIO.withConfigProvider(ConfigProvider.fromMap(Map.empty))(RepoRoot.load).flip
        empty     <- RepoRoot.from(" ").flip
        malformed <- RepoRoot.from("bad\u0000path").flip
      yield assertTrue(
        absent match
          case RepoRootError.Settings(_) => true
          case _                         => false,
        empty == RepoRootError.Empty,
        malformed match
          case RepoRootError.InvalidPath(_) => true
          case _                            => false,
      )
    },
    test("a directory without the Histos repository declaration is rejected") {
      ZIO.scoped:
        for
          path  <- repository
          _     <- ZIO.attemptBlockingIO(Files.delete(path.resolve("histos.scala")))
          error <- ZIO
            .withConfigProvider(ConfigProvider.fromMap(Map("HISTOS_REPO" -> path.toString)))(RepoRoot.load)
            .flip
        yield assertTrue(error == RepoRootError.NotRepository(path))
    },
  )

  private def repository: ZIO[Scope, java.io.IOException, Path] =
    ZIO.acquireRelease(
      for
        path <- ZIO.attemptBlockingIO(Files.createTempDirectory("heddle-docs-root-"))
        _    <- ZIO.attemptBlockingIO(Files.writeString(path.resolve("histos.scala"), ""))
        real <- ZIO.attemptBlockingIO(path.toRealPath())
      yield real
    )(path =>
      ZIO.attemptBlockingIO {
        Files.deleteIfExists(path.resolve("histos.scala"))
        Files.deleteIfExists(path)
        ()
      }.ignore
    )
end RepoRootSpec
