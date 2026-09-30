package heddle.mcp.apps.host

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import zio.{IO, ZIO}

/** A pin file on the local disk. The write lands in the same directory, then replaces the previous file. */
object PinFiles:
  def at(path: Path): PinFile = new PinFile:
    def read: IO[PinFileError, Option[String]] =
      ZIO
        .attempt {
          if Files.notExists(path) then None
          else Some(Files.readString(path, StandardCharsets.UTF_8))
        }
        .mapError(e => PinFileError.Unreadable(e.getClass.getSimpleName))

    def write(text: String): IO[PinFileError, Unit] =
      ZIO
        .attempt {
          val parent = path.getParent
          if parent != null then Files.createDirectories(parent)
          val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
          Files.writeString(tmp, text, StandardCharsets.UTF_8)
          try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          catch
            case _: AtomicMoveNotSupportedException =>
              Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
          ()
        }
        .mapError(e => PinFileError.Unwritable(e.getClass.getSimpleName))
end PinFiles
