package heddle.mcp.apps.host

import heddle.mcp.apps.*
import heddle.mcp.apps.ui.*
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*

object RemoteFrameSpec extends ZIOSpecDefault:
  import HostFixtures.*

  private val incAsk = ConsentRequest(counter, CounterHost.shed.uri, ToolName("inc"), Json.Obj())

  /** The browser's half, in this process: view messages one way, consent and resize the other. */
  private def browser(
      toBrowser: Queue[FrameEvent],
      toHost: Queue[FrameEvent],
      toView: Queue[Message],
      asked: Ref[Chunk[ConsentRequest]],
      resized: Promise[Nothing, Double],
      ready: Promise[Nothing, String],
  ) =
    ZStream
      .fromQueue(toBrowser)
      .foreach {
        case FrameEvent.Ready(html, _) => ready.succeed(html).unit
        case FrameEvent.ToView(message) => toView.offer(message).unit
        case FrameEvent.Ask(id, request) =>
          asked.update(_ :+ request) *> toHost.offer(FrameEvent.Answer(id, ConsentOutcome.AllowOnce)).unit
        case FrameEvent.Resize(height) => resized.succeed(height).unit
        case _                          => ZIO.unit
      }

  private val served = suite("a view served across a frame stream")(
    test("consent, the call, and a resize all cross it, and the server sees only the call"):
      for
        (mount, count) <- mounted()
        toBrowser      <- Queue.unbounded[FrameEvent]
        toHost         <- Queue.unbounded[FrameEvent]
        toView         <- Queue.unbounded[Message]
        asked          <- Ref.make(Chunk.empty[ConsentRequest])
        resized        <- Promise.make[Nothing, Double]
        ready          <- Promise.make[Nothing, String]
        _ <- browser(toBrowser, toHost, toView, asked, resized, ready).forkScoped
        remote <- RemoteFrame.connect(event => toBrowser.offer(event).unit, ZStream.fromQueue(toHost))
        _      <- remote.serve(mount)
        html   <- ready.await
        view = ViewPort.from(
          message => toHost.offer(FrameEvent.FromView(message)).unit,
          ZStream.fromQueue(toView),
        )
        bridge <- AppBridge.connect(CounterHost.shed, view, AppBridge.Settings(Implementation("counter-view", "1")))
        _      <- view.send(ViewNotification.SizeChanged(None, Some(400)).message)
        height <- resized.await
        out    <- bridge.call(_.inc)(())
        n      <- count.get
        who    <- asked.get
        log    <- ZIO.serviceWithZIO[Audit](_.history)
      yield assertTrue(
        html == mount.html,
        height == 400,
        out == Count(1),
        n == 1,
        who == Chunk(incAsk),
        log.collect { case AuditEvent(_, _, _, _, Action.Request("tools/call", _, _), d) => d } ==
          Chunk(Decision.ConsentAsked(ConsentOutcome.AllowOnce), Decision.Allowed),
      )
    ,
    test("a question still open when the browser goes away is unavailable"):
      for
        toBrowser <- Queue.unbounded[FrameEvent]
        toHost    <- Queue.unbounded[FrameEvent]
        remote    <- RemoteFrame.connect(event => toBrowser.offer(event).unit, ZStream.fromQueue(toHost))
        waiting   <- remote.ask(incAsk).fork
        sent      <- toBrowser.take
        _         <- toHost.shutdown
        outcome   <- waiting.join
      yield assertTrue(sent == FrameEvent.Ask(1, incAsk), outcome == ConsentOutcome.Unavailable)
    ,
  )

  private val codec = suite("FrameEvent")(
    test("each event round-trips, and a value that is not one does not"):
      val request = RelayRequest(SandboxGrant(), Csp.Scripts.AnyInline)
      val events  = List[FrameEvent](
        FrameEvent.Ready("<p>hi</p>", request),
        FrameEvent.ToView(Message.Notification("ping", Json.Obj("n" -> Json.Num(1)))),
        FrameEvent.FromView(Message.Result(RequestId.Num(7), Json.Obj())),
        FrameEvent.Resize(96),
        FrameEvent.OpenLink("https://example.test/a"),
        FrameEvent.Ask(3, incAsk),
        FrameEvent.Answer(3, ConsentOutcome.Rejected),
      )
      assertTrue(
        events.forall(event => event.toJson.fromJson[FrameEvent] == Right(event)),
        """{"Nope":{}}""".fromJson[FrameEvent].isLeft,
      )
  )

  /** A mounted counter, pinned, as `AppsHostSpec` mounts one. */
  private def mounted() =
    for
      (server, count) <- HostFixtures.server(HostFixtures.view)
      host            <- AppsHost.make(HostFixtures.settings)
      launch          <- HostFixtures.launched(server)
      mount           <- host.mount(server, launch)
    yield (mount, count)

  def spec = suite("RemoteFrame")(served, codec).provideSome[Scope](
    HashPins.inMemory,
    Audit.layer(),
    ConsentMemory.layer,
  ) @@ TestAspect.timeout(60.seconds)
end RemoteFrameSpec