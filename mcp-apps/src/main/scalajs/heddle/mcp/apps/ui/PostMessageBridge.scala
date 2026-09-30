package heddle.mcp.apps.ui

import ascent.dom
import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import scala.scalajs.js
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** What a view posts to and hears from. `listen` starts delivering each message's `data` and returns what stops it. */
trait PostTarget:
  def post(message: js.Any): Unit
  def listen(deliver: js.Any => Unit): () => Unit

object PostTarget:
  /** The view's host, `window.parent`. A sandboxed view's own origin is opaque, so it posts with target origin `*`, as
    * the ext-apps SDK does, and it hears only messages whose `source` is its parent.
    */
  def parentWindow: PostTarget = window(dom.window)

  def window(self: dom.Window): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = self.parent.foreach(_.postMessage(message, "*"))
    def listen(deliver: js.Any => Unit): () => Unit =
      messages(self)(e => if sentBy(e, self.parent) then deliver(e.data))

  /** One end of a `MessageChannel`: only the other end can post to it, so every message is the peer's. */
  def port(p: dom.MessagePort): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = p.postMessage(message)
    def listen(deliver: js.Any => Unit): () => Unit =
      val stop = messages(p)(e => deliver(e.data))
      p.start()
      stop

  /** Hands each `message` event `target` hears to `handle`, and answers what stops it. */
  private[heddle] def messages(target: dom.EventTarget)(handle: dom.MessageEvent => Unit): () => Unit =
    val listener: js.Function1[dom.Event, Unit] = {
      case e: dom.MessageEvent => handle(e)
      case _                   => ()
    }
    target.addEventListener("message", listener)
    () => target.removeEventListener("message", listener)

  /** Whether `window` sent `e`. A view's origin is opaque (`"null"`), so its window is the only proof of who spoke. */
  private[heddle] def sentBy(e: dom.MessageEvent, window: Option[dom.Window]): Boolean =
    e.source.exists(source => window.exists(js.special.strictEquals(source, _)))
end PostTarget

/** A `ViewPort` over `postMessage`. Messages cross as structured-clone JSON objects. Anything that is not a JSON-RPC
  * message is dropped, as the ext-apps SDK drops it.
  */
object PostMessageBridge:
  /** A view's port to its host: `window.parent`. */
  def toParent: ViewPort = over(PostTarget.parentWindow)

  def over(target: PostTarget): ViewPort = new ViewPort:
    // A value the browser cannot clone fails the post; that is the pipe failing.
    def send(message: Message): IO[McpError, Unit] =
      ZIO.attempt(target.post(js.JSON.parse(message.json.toJson))).mapError(McpError.Pipe(_))

    // The listener only hands each message to the stream's emitter, which is how a callback enters ZIO; it comes off
    // when the stream's scope closes.
    def receive: ZStream[Any, McpError, Message] =
      ZStream.asyncScoped[Any, McpError, Message] { emit =>
        ZIO.acquireRelease(
          ZIO.succeed(target.listen(data => deliver(emit, data)))
        )(stop => ZIO.succeed(stop()))
      }

  /** Every message from `target`, like `over(target).receive`, with `ready` run once the listener is in place, so the
    * answer to whatever `ready` prompts cannot arrive before anyone is listening. `ready` runs in the stream's scope:
    * what it forks lives as long as the stream.
    */
  def receiveThen(target: PostTarget)(ready: ZIO[Scope, McpError, Unit]): ZStream[Any, McpError, Message] =
    ZStream.asyncScoped[Any, McpError, Message] { emit =>
      ZIO.acquireRelease(ZIO.succeed(target.listen(data => deliver(emit, data))))(stop => ZIO.succeed(stop())) *>
        ready
    }

  /** Hands a JSON-RPC message to the stream; the emitter's answer says only whether it was queued. */
  private def deliver(
      emit: IO[Option[McpError], Chunk[Message]] => scala.concurrent.Future[Boolean],
      data: js.Any,
  ): Unit =
    decode(data) match
      case Some(message) =>
        val _ = emit(ZIO.succeed(Chunk.single(message)))
      case None => ()

  private def decode(data: js.Any): Option[Message] =
    js.JSON.stringify(data).fromJson[Json].toOption.flatMap(Message.decode(_).toOption)
end PostMessageBridge
