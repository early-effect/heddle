package heddle.mcp.apps.ui

import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import zio.*
import zio.stream.ZStream

/** One end of the conversation between a view and its host: `postMessage` in a browser, a pair of queues in tests, or a
  * stream to a host process that is not this page.
  */
trait ViewPort:
  def send(message: Message): IO[McpError, Unit]

  /** Every message from the other end, in order. It ends when the other end goes away. */
  def receive: ZStream[Any, McpError, Message]

object ViewPort:
  /** Two connected ends: what one sends, the other receives. For tests, and for a host and view in one process. */
  val pair: UIO[(ViewPort, ViewPort)] =
    (Queue.unbounded[Message] <*> Queue.unbounded[Message]).map { (toHost, toView) =>
      (from(toHost.offer(_).unit, ZStream.fromQueue(toView)), from(toView.offer(_).unit, ZStream.fromQueue(toHost)))
    }

  /** An end whose `send` is `send` and whose `receive` is `receive`. The other end is whoever feeds that stream. */
  def from(send: Message => IO[McpError, Unit], receive: ZStream[Any, McpError, Message]): ViewPort =
    Bound(send, receive)

  private final class Bound(out: Message => IO[McpError, Unit], in: ZStream[Any, McpError, Message]) extends ViewPort:
    def send(message: Message): IO[McpError, Unit] = out(message)
    def receive: ZStream[Any, McpError, Message]   = in
end ViewPort
