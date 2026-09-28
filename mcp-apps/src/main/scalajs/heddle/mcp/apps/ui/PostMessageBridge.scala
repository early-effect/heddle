package heddle.mcp.apps.ui

import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** The browser's `MessageEvent`: what arrived, which window sent it, and that window's origin (`"null"` if opaque). */
@js.native
trait MessageEvent extends js.Object:
  val data: js.Any   = js.native
  val source: js.Any = js.native
  val origin: String = js.native

/** A `MessagePort`, one end of a `MessageChannel`. */
@js.native
trait MessagePort extends js.Object:
  def postMessage(message: js.Any): Unit                                                  = js.native
  def addEventListener(kind: String, listener: js.Function1[MessageEvent, Unit]): Unit    = js.native
  def removeEventListener(kind: String, listener: js.Function1[MessageEvent, Unit]): Unit = js.native
  def start(): Unit                                                                       = js.native
  def close(): Unit                                                                       = js.native

@js.native
@JSGlobal
class MessageChannel() extends js.Object:
  val port1: MessagePort = js.native
  val port2: MessagePort = js.native

/** The parts of `window` a view uses. */
@js.native
trait Window extends js.Object:
  val parent: Window                                                                      = js.native
  def postMessage(message: js.Any, targetOrigin: String): Unit                            = js.native
  def addEventListener(kind: String, listener: js.Function1[MessageEvent, Unit]): Unit    = js.native
  def removeEventListener(kind: String, listener: js.Function1[MessageEvent, Unit]): Unit = js.native

@js.native
@JSGlobal("window")
private object BrowserWindow extends Window

/** What a view posts to and hears from. `listen` starts delivering each message's `data` and returns what stops it. */
trait PostTarget:
  def post(message: js.Any): Unit
  def listen(deliver: js.Any => Unit): () => Unit

object PostTarget:
  /** The view's host, `window.parent`. A sandboxed view's own origin is opaque, so it posts with target origin `*`, as
    * the ext-apps SDK does, and it hears only messages whose `source` is its parent.
    */
  def parentWindow: PostTarget = window(BrowserWindow)

  def window(self: Window): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = self.parent.postMessage(message, "*")
    def listen(deliver: js.Any => Unit): () => Unit =
      val listener: js.Function1[MessageEvent, Unit] = e =>
        if js.special.strictEquals(e.source, self.parent) then deliver(e.data)
      self.addEventListener("message", listener)
      () => self.removeEventListener("message", listener)

  /** One end of a `MessageChannel`: only the other end can post to it, so every message is the peer's. */
  def port(p: MessagePort): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = p.postMessage(message)
    def listen(deliver: js.Any => Unit): () => Unit =
      val listener: js.Function1[MessageEvent, Unit] = e => deliver(e.data)
      p.addEventListener("message", listener)
      p.start()
      () => p.removeEventListener("message", listener)
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
