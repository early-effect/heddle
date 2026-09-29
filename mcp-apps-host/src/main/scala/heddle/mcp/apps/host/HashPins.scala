package heddle.mcp.apps.host

import heddle.error.HeddleError
import heddle.mcp.apps.UiUri
import zio.*
import zio.json.*
import zio.json.ast.Json

/** Why a pin file could not be read, written, or trusted. */
enum PinFileError(val message: String) extends HeddleError:
  case Unreadable(detail: String) extends PinFileError(s"cannot read the pin file: $detail")
  case Unwritable(detail: String) extends PinFileError(s"cannot write the pin file: $detail")
  case Corrupt(detail: String)    extends PinFileError(s"pin file: $detail")

/** Which view's bytes a pin is for. */
final case class PinKey(server: ServerName, view: UiUri)

/** What a host found when it compared a view's bytes with its pin. */
enum PinCheck:
  case FirstSeen
  case Same
  case Changed(pinned: Digest)

/** The bytes of one pin file. A missing file is an empty store, not an error. */
trait PinFile:
  def read: IO[PinFileError, Option[String]]
  def write(text: String): IO[PinFileError, Unit]

/** The JSON array a [[PinFile]] stores: one object per view, `{server, view, digest}`. */
object Pins:
  def encode(pins: Map[PinKey, Digest]): String =
    val rows = pins.toList.sortBy { case (key, _) => (key.server.value, key.view.value) }
    Json
      .Arr(Chunk.fromIterable(rows.map { case (key, digest) =>
        Json.Obj(
          "server" -> Json.Str(key.server.value),
          "view"   -> Json.Str(key.view.value),
          "digest" -> Json.Str(digest.hex),
        )
      }))
      .toJson
  end encode

  def decode(text: String): Either[PinFileError, Map[PinKey, Digest]] =
    if text.trim.isEmpty then Right(Map.empty)
    else
      text.fromJson[Json].left.map(_ => PinFileError.Corrupt("not json")).flatMap {
        case Json.Arr(rows) =>
          rows.zipWithIndex.foldLeft[Either[PinFileError, Map[PinKey, Digest]]](Right(Map.empty)) {
            case (acc, (row, index)) =>
              acc.flatMap { soFar =>
                one(row, index).flatMap { (key, digest) =>
                  if soFar.contains(key) then
                    Left(PinFileError.Corrupt(s"row $index repeats ${key.server.value} ${key.view.value}"))
                  else Right(soFar.updated(key, digest))
                }
              }
          }
        case _ => Left(PinFileError.Corrupt("pins are a json array"))
      }

  private def one(row: Json, index: Int): Either[PinFileError, (PinKey, Digest)] =
    row match
      case obj: Json.Obj =>
        for
          server <- field(obj, "server", index).flatMap(s =>
            ServerName.from(s).left.map(e => PinFileError.Corrupt(s"row $index server: ${e.message}"))
          )
          view <- field(obj, "view", index).flatMap(s =>
            UiUri.from(s).left.map(e => PinFileError.Corrupt(s"row $index view: ${e.message}"))
          )
          digest <- field(obj, "digest", index).flatMap(s =>
            Digest.from(s).left.map(e => PinFileError.Corrupt(s"row $index digest: ${e.message}"))
          )
        yield (PinKey(server, view), digest)
      case _ => Left(PinFileError.Corrupt(s"row $index is not an object"))

  private def field(obj: Json.Obj, name: String, index: Int): Either[PinFileError, String] =
    obj.fields
      .collectFirst { case (`name`, Json.Str(s)) => s }
      .toRight(PinFileError.Corrupt(s"row $index has no string $name"))
end Pins

/** The SHA-256 of each view's bytes, pinned the first time the host reads them. Changed bytes refuse the mount until
  * the user accepts them.
  */
trait HashPins:
  /** Compares `served` with the pin, and pins it when there is none. */
  def check(key: PinKey, served: Digest): IO[PinFileError, PinCheck]

  /** The user accepted new bytes: they are the pin from now on. */
  def accept(key: PinKey, served: Digest): IO[PinFileError, Unit]

object HashPins:
  /** Pins that last as long as the layer. */
  val inMemory: ULayer[HashPins] =
    ZLayer(Ref.make(Map.empty[PinKey, Digest]).map { pins =>
      new HashPins:
        def check(key: PinKey, served: Digest): IO[PinFileError, PinCheck] =
          pins.modify { current =>
            current.get(key) match
              case None                   => (PinCheck.FirstSeen, current.updated(key, served))
              case Some(p) if p == served => (PinCheck.Same, current)
              case Some(p)                => (PinCheck.Changed(p), current)
          }

        def accept(key: PinKey, served: Digest): IO[PinFileError, Unit] = pins.update(_.updated(key, served))
    })

  /** Pins loaded from `file` when the layer opens, and written back on the first sight of a view and on `accept`. A
    * failed write leaves the previous pin in place. `PinFiles.at` is the file.
    */
  def durable(file: PinFile): ZLayer[Any, PinFileError, HashPins] =
    ZLayer {
      for
        text   <- file.read
        loaded <- ZIO.fromEither(text.fold[Either[PinFileError, Map[PinKey, Digest]]](Right(Map.empty))(Pins.decode))
        pins   <- Ref.make(loaded)
        gate   <- Semaphore.make(1)
      yield Durable(file, pins, gate)
    }

  private final class Durable(file: PinFile, pins: Ref[Map[PinKey, Digest]], gate: Semaphore) extends HashPins:
    def check(key: PinKey, served: Digest): IO[PinFileError, PinCheck] =
      gate.withPermit {
        pins.get.flatMap { current =>
          current.get(key) match
            case Some(pinned) if pinned == served => ZIO.succeed(PinCheck.Same)
            case Some(pinned)                     => ZIO.succeed(PinCheck.Changed(pinned))
            case None                             =>
              val next = current.updated(key, served)
              file.write(Pins.encode(next)) *> pins.set(next).as(PinCheck.FirstSeen)
        }
      }

    def accept(key: PinKey, served: Digest): IO[PinFileError, Unit] =
      gate.withPermit {
        pins.get.flatMap { current =>
          val next = current.updated(key, served)
          file.write(Pins.encode(next)) *> pins.set(next)
        }
      }
  end Durable
end HashPins
