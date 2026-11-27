package heddle.mcp.events

import zio.{IO, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

private[events] object SubscriptionStorePlatform:
  def file(path: String, capacity: Int): ZLayer[Any, StoreError, SubscriptionStore] =
    ZLayer.scoped {
      ZIO
        .attempt(Path.of(path))
        .mapError(e => StoreError.Unreadable(e.getClass.getSimpleName))
        .flatMap(file => SubscriptionStore.open(capacity, new FileJournal(file)))
    }

  private final class FileJournal(path: Path) extends SubscriptionStore.Journal:
    def load: IO[StoreError, Option[String]] =
      ZIO
        .attempt {
          if Files.notExists(path) then None
          else Some(Files.readString(path, StandardCharsets.UTF_8))
        }
        .mapError(e => StoreError.Unreadable(e.getClass.getSimpleName))

    def save(text: String): IO[StoreError, Unit] =
      ZIO
        .attempt {
          Option(path.getParent).foreach(parent => Files.createDirectories(parent))
          val name = Option(path.getFileName).map(_.toString).getOrElse("subscriptions")
          val tmp  = path.resolveSibling(name + ".tmp")
          Files.writeString(tmp, text, StandardCharsets.UTF_8)
          try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          catch
            case _: AtomicMoveNotSupportedException =>
              Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
          ()
        }
        .mapError(e => StoreError.Unwritable(e.getClass.getSimpleName))
  end FileJournal
end SubscriptionStorePlatform
