package heddle.mcp.apps.relay

import ascent.dom
import heddle.error.HeddleError
import heddle.mcp.apps.{Origin, Permission}
import heddle.mcp.apps.ui.*
import heddle.mcp.protocol.Message
import scala.scalajs.js
import zio.*
import zio.json.*
import zio.stream.ZStream

/** Why a relay framed nothing. Every case fails closed. */
enum RelayError(val message: String) extends HeddleError:
  case NotFramed        extends RelayError("a relay runs only inside its host's iframe")
  case SameOriginAsHost extends RelayError("the relay can read the top page, so it is not isolated from its host")
  case NoConfig         extends RelayError(s"there is no #${RelayConfig.ElementId} element")
  case NoBody           extends RelayError("the relay document has no body to frame the view in")
  case BadConfig(reason: String) extends RelayError(s"the relay config does not decode: $reason")

/** The sandbox proxy between a host page and one view.
  *
  * It trusts one origin, its parent's, as its config names it. It frames the view as `srcdoc` with
  * `sandbox="allow-scripts"` and the config's permissions, whatever `sandbox-resource-ready` says, and it forwards
  * JSON-RPC between the two, never a `ui/notifications/sandbox-*` message and nothing it made up. When the view's frame
  * loads a second document, forwarding stops for good and the host hears `sandbox-navigated` once.
  */
object Relay extends ZIOAppDefault:
  def run = relay(dom.window, dom.document).catchAll(e => ZIO.logError(s"heddle relay: ${e.message}"))

  def relay(self: dom.Window, document: dom.Document): IO[RelayError, Unit] =
    for
      _      <- isolated(self)
      config <- read(document)
      body   <- ZIO.fromOption(document.body).orElseFail(RelayError.NoBody)
      frame  = document.createElement(dom.HtmlTag.iframe)
      toHost = parent(self, config.host)
      toView = child(self, frame)
      host   = PostMessageBridge.over(toHost)
      view   = PostMessageBridge.over(toView)
      started <- Promise.make[Nothing, Unit]
      stopped <- Promise.make[Nothing, Unit]
      forward = (to: ViewPort) => (m: Message) => ZIO.unlessZIODiscard(stopped.isDone)(to.send(m).ignore)

      /** The frame goes in once the view's listener is in place, so the view's first message is heard. */
      mount = (html: String) =>
        PostMessageBridge
          .receiveThen(toView)(
            ZIO.succeed(prepare(frame, html, config.grant.permissions)) *>
              loads(frame, body).zipWithIndex
                .foreach((_, n) => ZIO.whenZIODiscard(ZIO.succeed(n > 0) && stopped.succeed(()))(navigated(host)))
                .forkScoped
                .unit
          )
          .filter(relayable)
          .foreach(forward(host))
          .ignore
          .forkScoped
          .unit

      _ <- ZIO.scoped(
        PostMessageBridge
          .receiveThen(toHost)(host.send(SandboxMessage.ProxyReady.message))
          .foreach {
            case Message.Notification(method, params) if method.startsWith(SandboxMessage.Prefix) =>
              SandboxMessage.decode(method, params) match
                case Right(SandboxMessage.ResourceReady(html, _, _)) =>
                  ZIO.whenZIODiscard(started.succeed(()))(mount(html))
                case _ => ZIO.unit
            case m => ZIO.whenZIODiscard(started.isDone)(forward(view)(m))
          }
          .ignore
      )
    yield ()

  /** A relay that can read its top page shares an origin with its host, so its view could too. */
  private def isolated(self: dom.Window): IO[RelayError, Unit] =
    self.top match
      case Some(top) if !js.special.strictEquals(self.self, top) =>
        // The browser answers a cross-origin read by throwing; that throw is the proof of isolation.
        ZIO.attempt(top.location.href).foldZIO(_ => ZIO.unit, _ => ZIO.fail(RelayError.SameOriginAsHost))
      case _ => ZIO.fail(RelayError.NotFramed)

  private def read(document: dom.Document): IO[RelayError, RelayConfig] =
    ZIO
      .fromOption(document.getElementById(RelayConfig.ElementId).flatMap(_.textContent))
      .orElseFail(RelayError.NoConfig)
      .flatMap(text => ZIO.fromEither(text.fromJson[RelayConfig]).mapError(RelayError.BadConfig(_)))

  private def relayable(m: Message): Boolean =
    m match
      case Message.Request(_, method, _)   => !method.startsWith(SandboxMessage.Prefix)
      case Message.Notification(method, _) => !method.startsWith(SandboxMessage.Prefix)
      case _                               => true

  private def navigated(host: ViewPort): UIO[Unit] = host.send(SandboxMessage.Navigated.message).ignore

  private def prepare(frame: dom.HTMLIFrameElement, html: String, permissions: Set[Permission]): Unit =
    frame.setAttribute("sandbox", "allow-scripts")
    Permission.allow(permissions).foreach(frame.setAttribute("allow", _))
    frame.setAttribute("style", "display:block;width:100%;height:100%;border:0")
    frame.srcdoc = html

  /** Inserts `frame` when the stream starts and removes it when the stream ends; one element per document it loads. */
  private def loads(frame: dom.HTMLIFrameElement, parent: dom.Node): ZStream[Any, Nothing, Unit] =
    ZStream.asyncScoped[Any, Nothing, Unit] { emit =>
      ZIO.acquireRelease(ZIO.succeed {
        val listener: js.Function1[dom.Event, Unit] = _ =>
          val _ = emit(ZIO.succeed(Chunk.unit))
        frame.addEventListener("load", listener)
        val _ = parent.appendChild(frame)
        listener
      })(listener => ZIO.succeed { frame.removeEventListener("load", listener); frame.remove() })
    }

  /** The host page: posted to at its origin, and heard only from that window at that origin. */
  private def parent(self: dom.Window, host: Origin): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = self.parent.foreach(_.postMessage(message, host.render))
    def listen(deliver: js.Any => Unit): () => Unit =
      PostTarget.messages(self)(e =>
        if PostTarget.sentBy(e, self.parent) && e.origin == host.render then deliver(e.data)
      )

  /** The view: opaque, so posted to with `*`, and heard only from that frame's window, whose origin is `null`. */
  private def child(self: dom.Window, frame: dom.HTMLIFrameElement): PostTarget = new PostTarget:
    def post(message: js.Any): Unit                 = frame.contentWindow.foreach(_.postMessage(message, "*"))
    def listen(deliver: js.Any => Unit): () => Unit =
      PostTarget.messages(self)(e =>
        if PostTarget.sentBy(e, frame.contentWindow) && e.origin == "null" then deliver(e.data)
      )
end Relay
