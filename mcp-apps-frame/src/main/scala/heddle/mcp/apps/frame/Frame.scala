package heddle.mcp.apps.frame

import ascent.dom
import heddle.error.HeddleError
import heddle.http.{Scheme, Url}
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
  case HostNeverReady(waited: Duration)  extends FrameError(s"the host did not send a frame within $waited")
  case Pipe(error: McpError)             extends FrameError(error.message)

/** The relay iframe and its handshake, back from `open`. Serving it is a separate step, so the pipeline can run in
  * another process.
  */
final case class Opened(port: ViewPort, frame: ViewFrame)

/** The host page's half of the sandbox: one relay iframe and its handshake. */
object Frame:
  /** Frames `mount` under `parent` and serves it in this process until it ends. `parent` may be an element or a shadow
    * root; heddle makes the iframe, sets every attribute on it, and puts it in once it is listening. The iframe goes
    * when the scope closes. `host` is this page's origin, the only one the relay will talk to.
    */
  def mount(
      mount: Mount,
      mode: RelayMode,
      parent: dom.Node,
      host: Origin,
      ready: Duration = 10.seconds,
  ): ZIO[Scope & ConsentGate & Audit, FrameError, Mounted] =
    open(mount.html, RelayRequest.of(mount), mode, parent, host, ready).flatMap { opened =>
      mount.serve(opened.port, opened.frame)
    }

  /** Frames the relay and completes its handshake, and does not serve. `serve` the returned port where the pipeline
    * runs. The iframe goes when the scope closes.
    */
  def open(
      html: String,
      request: RelayRequest,
      mode: RelayMode,
      parent: dom.Node,
      host: Origin,
      ready: Duration = 10.seconds,
  ): ZIO[Scope, FrameError, Opened] =
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
      opened  = Opened(
        ViewPort.from(toRelay.send, ZStream.fromQueue(heard)),
        page(frame),
      )
      _ <- opened.port
        .send(SandboxMessage.ResourceReady(html, None, request.grant).message)
        .mapError(FrameError.Pipe(_))
    yield opened

  /** Frames whatever the host sends on `incoming`, and carries the view's messages back through `send`. The pipeline
    * stays wherever `RemoteFrame.serve` runs. `onAsk` is told each question; the page sends the user's `Answer`. An
    * `OpenLink` that is not http or https is dropped. The iframe goes when the scope closes.
    */
  def follow(
      parent: dom.Node,
      host: Origin,
      mode: RelayMode,
      send: FrameEvent => IO[McpError, Unit],
      incoming: ZStream[Any, McpError, FrameEvent],
      onAsk: (Long, ConsentRequest) => UIO[Unit],
      ready: Duration = 10.seconds,
  ): ZIO[Scope, FrameError, Opened] =
    for
      rest    <- Queue.unbounded[FrameEvent]
      arrived <- Promise.make[FrameError, FrameEvent.Ready]
      _       <- incoming
        .foreach {
          case event: FrameEvent.Ready => arrived.succeed(event).unit
          case event                   => rest.offer(event).unit
        }
        .catchAll(error => arrived.fail(FrameError.Pipe(error)).unit)
        .ensuring(ZIO.unlessZIO(arrived.isDone)(arrived.fail(FrameError.HostNeverReady(ready)).unit))
        .forkScoped
      document <- arrived.await.timeoutFail(FrameError.HostNeverReady(ready))(ready)
      opened   <- open(document.html, document.request, mode, parent, host, ready)
      _        <- ZStream
        .fromQueue(rest)
        .foreach {
          case FrameEvent.ToView(message)  => opened.port.send(message).ignore
          case FrameEvent.Resize(height)   => opened.frame.resize(height)
          case FrameEvent.OpenLink(url)    => ZIO.foreachDiscard(link(url))(opened.frame.openLink)
          case FrameEvent.Ask(id, request) => onAsk(id, request).forkScoped.unit
          case _                           => ZIO.unit
        }
        .forkScoped
      _ <- opened.port.receive.foreach(message => send(FrameEvent.FromView(message))).ignore.forkScoped
    yield opened

  /** http and https only, absolute: the same links the pipeline will ask a page to open. */
  private def link(raw: String): Option[Url] =
    Url.decode(raw).toOption.filter(u => u.absolute && u.scheme.exists(s => s == Scheme.Http || s == Scheme.Https))

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
