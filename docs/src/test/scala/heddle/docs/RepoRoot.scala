package heddle.docs

import zio.*

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.util.Try

enum RepoRootError:
  case Settings(error: Config.Error)
  case Empty
  case InvalidPath(cause: Throwable)
  case NotRepository(path: Path)
  case Unreadable(path: Path, cause: IOException)

/** The explicit repository root supplied by the runner or a direct test invocation. */
final class RepoRoot private (val path: Path)

object RepoRoot:
  private val config = Config.string("root").nested("source").nested("specular").orElse(Config.string("HISTOS_REPO"))

  val load: IO[RepoRootError, RepoRoot] =
    ZIO.config(config).mapError(RepoRootError.Settings(_)).flatMap(from)

  def from(raw: String): IO[RepoRootError, RepoRoot] =
    for
      path  <- ZIO.fromEither(parse(raw))
      real  <- ZIO.attemptBlockingIO(path.toRealPath()).mapError(RepoRootError.Unreadable(path, _))
      valid <- ZIO
        .attemptBlockingIO(Files.isRegularFile(real.resolve("histos.scala")))
        .mapError(RepoRootError.Unreadable(real, _))
      _ <- ZIO.fail(RepoRootError.NotRepository(real)).unless(valid)
    yield new RepoRoot(real)

  private def parse(raw: String): Either[RepoRootError, Path] =
    if raw.trim.isEmpty then Left(RepoRootError.Empty)
    else Try(Path.of(raw.trim).toAbsolutePath.normalize).toEither.left.map(RepoRootError.InvalidPath(_))
end RepoRoot
