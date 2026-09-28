package heddle.mcp.apps.frame

import ascent.dom
import heddle.error.HeddleError
import heddle.http.Url
import heddle.mcp.apps.{Origin, Permission}
import heddle.mcp.apps.host.*
import heddle.mcp.apps.ui.*
import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import scala.scalajs.js
import zio.*
import zio.stream.ZStream

/** Where a view's relay lives. */
enum RelayMode:
  /** On its own origin, served by `RelayRoutes` with its CSP in the header and `frame-ancestors` naming this page. */
  case Served(relay: Origin)

  /** A `srcdoc` this page writes, with its CSP as the first `<meta>`: for a host with no second origin to serve from.
    */
  case Opaque

/** Why a frame did not start. */
enum FrameError(val message: String) extends HeddleError:
  case RelayNeverReady(waited: Duration) extends FrameError(s"the relay did not say it was ready within $waited")
  case Pipe(error: McpError)             extends FrameError(error.message)

/** The host page's half of the sandbox: one relay iframe, its handshake, and the mount served through it. */
object Frame:
  /** Frames `mount` under `parent` and serves it until it ends. `parent` may be an element or a shadow root; heddle
    * makes the iframe, sets every attribute on it, and puts it in once it is listening. The iframe goes when the scope
    * closes. `host` is this page's origin, the only one the relay will talk to.
    */
  def mount(
      mount: Mount,
      mode: RelayMode,
      parent: dom.Node,
      host: Origin,
      ready: Duration = 10.seconds,
  ): ZIO[Scope & ConsentGate & Audit, FrameError, Mounted] =
    val request = RelayRequest.of(mount)
    for
      frame <- ZIO.succeed(dom.document.createElement(dom.HtmlTag.iframe))
      target = relay(frame, mode)
      heard <- Queue.unbounded[Message]
      // One listener for the frame's life, in place before the iframe is, so nothing the relay says is missed.
      _ <- PostMessageBridge
        .receiveThen(target)(
          ZIO.acquireRelease(ZIO.succeed(insert(frame, mode, request, parent, host)))(_ => ZIO.succeed(frame.remove()))
        )
        .foreach(heard.offer)
        .ignore
        .forkScoped
      _ <- ZStream
        .fromQueue(heard)
        .collect { case Message.Notification(m, _) if m == SandboxMessage.ProxyReady.method => () }
        .runHead
        .someOrFail(FrameError.RelayNeverReady(ready))
        .timeoutFail(FrameError.RelayNeverReady(ready))(ready)
      toRelay = PostMessageBridge.over(target)
      port    = new ViewPort:
        def send(message: Message): IO[McpError, Unit] = toRelay.send(message)
        def receive: ZStream[Any, McpError, Message]   = ZStream.fromQueue(heard)
      served <- mount.serve(port, page(frame))
      _ <- port.send(SandboxMessage.ResourceReady(mount.html, None, mount.grant).message).mapError(FrameError.Pipe(_))
    yield served
    end for
  end mount

  private def insert(
      frame: dom.HTMLIFrameElement,
      mode: RelayMode,
      request: RelayRequest,
      parent: dom.Node,
      host: Origin,
  ): Unit =
    mode match
      case RelayMode.Served(relay) =>
        frame.setAttribute("sandbox", "allow-scripts allow-same-origin")
        frame.src = RelayRoutes.url(relay, request)
      case RelayMode.Opaque =>
        frame.setAttribute("sandbox", "allow-scripts")
        frame.srcdoc = RelayDocument.opaque(host, request)
    // The relay can only delegate what its own frame was allowed.
    Permission.allow(request.grant.permissions).foreach(frame.setAttribute("allow", _))
    frame.setAttribute("style", "display:block;width:100%;height:96px;border:0")
    val _ = parent.appendChild(frame)
  end insert

  /** The relay: posted to at its origin, and heard only from its window at that origin (`null` when opaque). */
  private def relay(frame: dom.HTMLIFrameElement, mode: RelayMode): PostTarget =
    val origin = mode match
      case RelayMode.Served(relay) => relay.render
      case RelayMode.Opaque        => "null"
    val target = mode match
      case RelayMode.Served(relay) => relay.render
      case RelayMode.Opaque        => "*"
    new PostTarget:
      def post(message: js.Any): Unit                 = frame.contentWindow.foreach(_.postMessage(message, target))
      def listen(deliver: js.Any => Unit): () => Unit =
        PostTarget.messages(dom.window)(e =>
          if PostTarget.sentBy(e, frame.contentWindow) && e.origin == origin then deliver(e.data)
        )
  end relay

  /** What the page does for the view: size its frame, and open a link in a new, unrelated tab. */
  private def page(frame: dom.HTMLIFrameElement): ViewFrame = new ViewFrame:
    def resize(height: Double): UIO[Unit] =
      ZIO.succeed(frame.setAttribute("style", s"display:block;width:100%;height:${height}px;border:0"))
    def openLink(url: Url): UIO[Unit] =
      ZIO.succeed(dom.window.open(url.render, "_blank", "noopener,noreferrer")).unit
end Frame
