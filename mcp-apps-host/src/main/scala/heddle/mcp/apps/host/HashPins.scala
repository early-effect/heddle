package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import zio.*

/** Which view's bytes a pin is for. */
final case class PinKey(server: ServerName, view: UiUri)

/** What a host found when it compared a view's bytes with its pin. */
enum PinCheck:
  case FirstSeen
  case Same
  case Changed(pinned: Digest)

/** The SHA-256 of each view's bytes, pinned the first time the host reads them. Changed bytes refuse the mount until
  * the user accepts them.
  */
trait HashPins:
  /** Compares `served` with the pin, and pins it when there is none. */
  def check(key: PinKey, served: Digest): UIO[PinCheck]

  /** The user accepted new bytes: they are the pin from now on. */
  def accept(key: PinKey, served: Digest): UIO[Unit]

object HashPins:
  /** Pins that last as long as the layer. An adapter with a file store keeps them across sessions. */
  val inMemory: ULayer[HashPins] =
    ZLayer(Ref.make(Map.empty[PinKey, Digest]).map { pins =>
      new HashPins:
        def check(key: PinKey, served: Digest): UIO[PinCheck] =
          pins.modify { current =>
            current.get(key) match
              case None                   => (PinCheck.FirstSeen, current.updated(key, served))
              case Some(p) if p == served => (PinCheck.Same, current)
              case Some(p)                => (PinCheck.Changed(p), current)
          }

        def accept(key: PinKey, served: Digest): UIO[Unit] = pins.update(_.updated(key, served))
    })
end HashPins
