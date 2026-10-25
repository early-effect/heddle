package heddle.mcp

import heddle.mcp.protocol.Message
import zio.{FiberRef, UIO, Unsafe, ZIO}

/** The request that is running the current effect. A tool calls [[publish]] and does not name the pipe or HTTP. */
object Session:
  private val outbound: FiberRef[Option[Message.Notification => UIO[Unit]]] =
    // One process-wide ref, built the same way ZIO builds its own. `FiberRef.make` is an effect.
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.make(None)
    }

  /** Queue one JSON-RPC notification on this request. Completes when that notification is a whole message ahead of this
    * request's response. Outside a request, succeeds and sends nothing.
    */
  def publish(notification: Message.Notification): UIO[Unit] =
    outbound.get.flatMap {
      case Some(send) => send(notification)
      case None       => ZIO.unit
    }

  /** Install `send` for `effect` and for fibers forked from it. The transport owns `send`. */
  private[mcp] def bind[R, E, A](send: Message.Notification => UIO[Unit])(effect: ZIO[R, E, A]): ZIO[R, E, A] =
    outbound.locally(Some(send))(effect)
end Session
