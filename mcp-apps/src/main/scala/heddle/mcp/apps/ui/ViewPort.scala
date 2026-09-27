package heddle.mcp.apps.ui

import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import zio.*
import zio.stream.ZStream

/** One end of the conversation between a view and its host: `postMessage` in a browser, a pair of queues in tests. */
trait ViewPort:
  def send(message: Message): IO[McpError, Unit]

  /** Every message from the other end, in order. It ends when the other end goes away. */
  def receive: ZStream[Any, McpError, Message]

object ViewPort:
  /** Two connected ends: what one sends, the other receives. For tests, and for a host and view in one process. */
  val pair: UIO[(ViewPort, ViewPort)] =
    (Queue.unbounded[Message] <*> Queue.unbounded[Message]).map { (toHost, toView) =>
      (Queued(toHost, toView), Queued(toView, toHost))
    }

  private final class Queued(out: Queue[Message], in: Queue[Message]) extends ViewPort:
    def send(message: Message): IO[McpError, Unit] = out.offer(message).unit
    def receive: ZStream[Any, McpError, Message]   = ZStream.fromQueue(in)
end ViewPort
