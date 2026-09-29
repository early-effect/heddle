package heddle.mcp.apps.host

import heddle.http.Url
import heddle.mcp.apps.ui.ViewPort
import heddle.mcp.client.McpError
import heddle.mcp.protocol.Message
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** What one mounted view says between the process that runs `Mount.serve` and the browser that only frames it.
  *
  * Toward the browser: `Ready` once, then `ToView`, `Resize`, `OpenLink`, and `Ask`. Toward the host: `FromView` and
  * `Answer`. The browser builds the relay document from `Ready` with its own origin, so the stream never carries a CSP
  * string.
  */
enum FrameEvent:
  case Ready(html: String, request: RelayRequest)
  case ToView(message: Message)
  case FromView(message: Message)
  case Resize(heightPx: Double)
  case OpenLink(url: String)
  case Ask(id: Long, request: ConsentRequest)
  case Answer(id: Long, outcome: ConsentOutcome)

object FrameEvent:
  /** JSON-RPC, embedded as its wire object. Not a derived codec: `Message` is not a sum zio-json can write. */
  given JsonCodec[Message] =
    JsonCodec[Json].transformOrFail(
      json => Message.decode(json).left.map(_.error.text),
      _.json,
    )

  given JsonCodec[FrameEvent] = JsonCodec.derived
end FrameEvent

/** The host-process end of a [[FrameEvent]] stream. `Mount.serve` runs here; the browser holds the iframe. */
final class RemoteFrame private (
    send: FrameEvent => IO[McpError, Unit],
    val port: ViewPort,
    val frame: ViewFrame,
    val ask: ConsentRequest => UIO[ConsentOutcome],
):
  /** Tells the browser what to frame, then serves `mount` until the stream ends. */
  def serve(mount: Mount): ZIO[Scope & ConsentMemory & Audit, McpError, Mounted] =
    for
      _      <- send(FrameEvent.Ready(mount.html, RelayRequest.of(mount)))
      mem    <- ZIO.service[ConsentMemory]
      served <- mount
        .serve(port, frame)
        .provideSome[Scope & Audit](ZLayer.succeed(mem.gate(ask)))
    yield served
end RemoteFrame

object RemoteFrame:
  /** `send` goes to the browser. `incoming` comes from it. An incoming failure or end closes the port and answers any
    * question still open with `Unavailable`.
    */
  def connect(
      send: FrameEvent => IO[McpError, Unit],
      incoming: ZStream[Any, McpError, FrameEvent],
  ): URIO[Scope, RemoteFrame] =
    for
      fromView <- Queue.unbounded[Message]
      answers  <- Ref.make(Map.empty[Long, Promise[Nothing, ConsentOutcome]])
      next     <- Ref.make(0L)
      closed   <- Promise.make[Nothing, Unit]
      stop = closed.succeed(()).unit *> answers
        .getAndSet(Map.empty)
        .flatMap(pending => ZIO.foreachDiscard(pending.values)(_.succeed(ConsentOutcome.Unavailable)))
      _ <- incoming
        .foreach {
          case FrameEvent.FromView(message)   => fromView.offer(message).unit
          case FrameEvent.Answer(id, outcome) =>
            answers
              .modify(pending => (pending.get(id), pending - id))
              .flatMap(ZIO.foreachDiscard(_)(_.succeed(outcome)))
          case _ => ZIO.unit
        }
        .catchAll(_ => ZIO.unit)
        .ensuring(stop *> fromView.shutdown)
        .forkScoped
      frame = new ViewFrame:
        def resize(height: Double): UIO[Unit] = send(FrameEvent.Resize(height)).ignore
        def openLink(url: Url): UIO[Unit]     = send(FrameEvent.OpenLink(url.render)).ignore
      ask = (request: ConsentRequest) =>
        for
          id      <- next.updateAndGet(_ + 1)
          waiting <- Promise.make[Nothing, ConsentOutcome]
          _       <- answers.update(_.updated(id, waiting))
          _       <- ZIO.whenZIO(closed.isDone)(
            answers.update(_ - id) *> waiting.succeed(ConsentOutcome.Unavailable)
          )
          sent <- send(FrameEvent.Ask(id, request)).either
          _    <- ZIO.when(sent.isLeft)(
            answers.update(_ - id) *> waiting.succeed(ConsentOutcome.Unavailable)
          )
          outcome <- waiting.await
        yield outcome
    yield new RemoteFrame(
      send,
      ViewPort.from(message => send(FrameEvent.ToView(message)), ZStream.fromQueue(fromView)),
      frame,
      ask,
    )
end RemoteFrame
