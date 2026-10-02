package heddle.mcp.apps.host

import heddle.mcp.apps.*
import heddle.mcp.apps.ui.*
import heddle.mcp.client.{McpCallFailure, McpClient, McpError}
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*

object AppsHostSpec extends ZIOSpecDefault:
  import HostFixtures.*

  private val connect = Origin("https://api.test")

  /** A mounted view and the server's count behind it. */
  private def mounted(
      document: UiDocument = view,
      policy: UiPolicy = UiPolicy.closed,
      allow: HostPolicy = HostPolicy.open,
  ) =
    for
      (s, count) <- server(document, policy)
      host       <- AppsHost.make(settings.copy(policy = allow))
      launch     <- launched(s)
      m          <- host.mount(s, launch)
    yield (m, count)

  private def audited: URIO[Audit, Chunk[AuditEvent]] = ZIO.serviceWithZIO[Audit](_.history)

  /** The audit's decisions about `tools/call`, in order. */
  private def calls(log: Chunk[AuditEvent]): Chunk[Decision] =
    log.collect { case AuditEvent(_, _, _, _, Action.Request("tools/call", _, _), d) => d }

  private def refusal(id: Long, denial: Denial): Message =
    Message.Error(Some(RequestId.Num(id)), RpcError.Other(-32000, denial.message, None))

  /** A heddle view on one end of a pair, the host serving the other. */
  private def served(gate: ConsentGate) =
    for
      (m, count)   <- mounted()
      (view, back) <- ViewPort.pair
      p            <- page
      _            <- m.serve(back, p).provideSome[Scope & Audit](ZLayer.succeed(gate))
      bridge       <- AppBridge.connect(CounterHost.shed, view, AppBridge.Settings(Implementation("counter-view", "1")))
    yield (bridge, m, count)

  /** A raw view on one end, the host serving the other, past the handshake. */
  private def raw(gate: ConsentGate) =
    for
      (m, count)  <- mounted()
      (view, end) <- rawPair
      p           <- page
      served      <- m.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(gate))
      _           <- view.ask(0, "ui/initialize", initialize)
      _           <- view.notify("ui/notifications/initialized")
    yield (view, served, count, p)

  private val mountSuite = suite("mount")(
    test("the view's tools are exactly the app-visible tools linked to it on this server"):
      mounted().map((m, _) =>
        assertTrue(
          m.view.uri == CounterHost.shed.uri,
          m.view.launch.name == ToolName("show_counter"),
          m.view.appTools == Set(ToolName("show_counter"), ToolName("inc"), ToolName("reset")),
          !m.view.admits(ToolName("bash")),
        )
      )
    ,
    test("the effective policy is the view's ask clamped by the host, and every narrowing is audited"):
      val ask = UiPolicy(network = Network(connect = Set(connect)), permissions = Set(Permission.Camera))
      for
        (m, _) <- mounted(policy = ask, allow = HostPolicy.closed)
        log    <- audited
      yield assertTrue(
        m.policy == Clamp[UiPolicy, HostPolicy].clamp(ask, HostPolicy.closed),
        m.grant == SandboxGrant(),
        log.collect { case AuditEvent(_, _, _, _, Action.Mount, Decision.Narrowed(n)) => n }.toSet ==
          Set(Narrowing.OriginDropped(Directive.Connect, connect), Narrowing.PermissionDropped(Permission.Camera)),
        log.lastOption.map(_.decision).contains(Decision.Allowed),
      )
      end for
    ,
    test("changed bytes are refused and audited until the user accepts them"):
      val changed = UiDocument("Counter", "window.counter = 2;")
      for
        _       <- mounted(view)
        refused <- mounted(changed).flip
        log     <- audited
        _     <- ZIO.serviceWithZIO[HashPins](_.accept(PinKey(counter, CounterHost.shed.uri), Digest.of(changed.html)))
        again <- mounted(changed)
      yield assertTrue(
        refused == MountRefusal.HashMismatch(CounterHost.shed.uri, Digest.of(view.html), Digest.of(changed.html)),
        log.exists(_.decision == Decision.HashMismatch(Digest.of(view.html), Digest.of(changed.html))),
        again._1.html == changed.html,
      )
      end for
    ,
    test("a launch tool with no view, or one the server does not list, mounts nothing"):
      for
        (s, _)   <- server(view)
        host     <- AppsHost.make(settings)
        result   <- s.session.callTool(ToolName("bash"), Json.Obj())
        noView   <- host.mount(s, Launched(ToolName("bash"), Json.Obj(), result)).flip
        unlisted <- host.mount(s, Launched(ToolName("rm_rf"), Json.Obj(), result)).flip
      yield assertTrue(
        noView == MountRefusal.NoView(ToolName("bash")),
        unlisted == MountRefusal.NotListed(ToolName("rm_rf")),
      )
    ,
    test("a view resource must be the linked uri, typed as an app, as text or base64"):
      val uri  = CounterHost.shed.uri
      val html = "<!doctype html><p>hi</p>"
      val blob = java.util.Base64.getEncoder.encodeToString(html.getBytes("UTF-8"))
      val app  = Some(UiMeta.MimeType)
      assertTrue(
        ViewResource.of(uri, Chunk(ResourceContents.Blob(uri.value, app, blob, None))).map(_.html) == Right(html),
        ViewResource.of(uri, Chunk(ResourceContents.Text(uri.value, Some("text/html"), html, None))) ==
          Left(MountRefusal.NotAnApp(uri, Some("text/html"))),
        ViewResource.of(uri, Chunk(ResourceContents.Text("ui://other/view", app, html, None))).isLeft,
        ViewResource.of(uri, Chunk(ResourceContents.Blob(uri.value, app, "not base64!", None))).isLeft,
      ),
  )

  private val serveSuite = suite("serve")(
    test("a heddle view's handshake gets the host, the grant it may use, and the tool that opened it; then its run"):
      for
        (gate, _)      <- answering(ConsentOutcome.Unavailable)
        (bridge, m, _) <- served(gate)
        returned       <- bridge.run.collect { case r: Run.Returned[?, ?, ?] => r }.runHead
      yield assertTrue(
        bridge.host.hostInfo == settings.info,
        bridge.host.hostCapabilities.sandbox.contains(m.grant),
        bridge.host.hostContext.toolInfo.map(_.tool.name).contains(ToolName("show_counter")),
        returned.isDefined,
      )
    ,
    test("a call reaches the server only after consent, and the audit records the ask and the call"):
      for
        (gate, asked)      <- answering(ConsentOutcome.AllowOnce)
        (bridge, _, count) <- served(gate)
        out                <- bridge.call(_.inc)(())
        n                  <- count.get
        who                <- asked.get
        log                <- audited
      yield assertTrue(
        out == Count(1),
        n == 1,
        who.map(_.tool) == Chunk(ToolName("inc")),
        who.map(_.summary) == Chunk(Some("Increment")),
        calls(log) == Chunk(Decision.ConsentAsked(ConsentOutcome.AllowOnce), Decision.Allowed),
      )
    ,
    test("a call the user does not allow never reaches the server"):
      checkAll(Gen.fromIterable(List(ConsentOutcome.Rejected, ConsentOutcome.Cancelled, ConsentOutcome.Unavailable))) {
        outcome =>
          ZIO
            .scoped(
              for
                (gate, _)          <- answering(outcome)
                (bridge, _, count) <- served(gate)
                result             <- bridge.call(_.inc)(()).either
                n                  <- count.get
              yield assertTrue(
                result == Left(
                  McpCallFailure.Session(
                    McpError.Rpc(RpcError.Other(-32000, Denial.ConsentRefused(outcome).message, None))
                  )
                ),
                n == 0,
              )
            )
            .provideSome[HashPins](Audit.layer())
      }
    ,
    test("a tool that is not linked to the view is refused before anyone is asked"):
      checkAll(Gen.fromIterable(List(ToolName("bash"), ToolName("reset_all"), ToolName("mcp__other__rm")))) { name =>
        ZIO
          .scoped(
            for
              (gate, asked)    <- answering(ConsentOutcome.AllowForSession)
              (v, _, count, _) <- raw(gate)
              reply            <- v.ask(7, "tools/call", Json.Obj("name" -> Json.Str(name.value)))
              who              <- asked.get
              n                <- count.get
              log              <- audited
            yield assertTrue(
              reply == refusal(7, Denial.NotLinked(name)),
              who.isEmpty,
              n == 0,
              calls(log) == Chunk(Decision.Denied(Denial.NotLinked(name))),
            )
          )
          .provideSome[HashPins](Audit.layer())
      }
    ,
    test("a view can read any resource on its server"):
      for
        (gate, _)    <- answering(ConsentOutcome.Unavailable)
        (v, _, _, _) <- raw(gate)
        other        <- v.ask(1, "resources/read", Json.Obj("uri" -> Json.Str("ui://other/secret")))
        own          <- v.ask(2, "resources/read", Json.Obj("uri" -> Json.Str(CounterHost.shed.uri.value)))
        ownUris = own match
          case Message.Result(_, r) => r.as[ReadResourceResult].toOption.map(_.contents.map(_.uri))
          case _                    => None
        missing = other match
          case Message.Error(_, RpcError.ResourceNotFound(message)) => message.contains("ui://other/secret")
          case _                                                    => false
      yield assertTrue(
        missing,
        ownUris.contains(Chunk(CounterHost.shed.uri.value)),
      )
    ,
    test("subscribe is forwarded only when the host offers it, and a list change reaches the view"):
      val uri = Json.Obj("uri" -> Json.Str(CounterHost.shed.uri.value))
      for
        (gate, _)         <- answering(ConsentOutcome.AllowForSession)
        (closed, _, _, _) <- raw(gate)
        refused           <- closed.ask(1, "resources/subscribe", uri)
        (openGate, _)     <- answering(ConsentOutcome.AllowForSession)
        (mcp, live, _)    <- serving(view, handshake = McpClient.Handshake.Session)
        host              <- AppsHost.make(settings.copy(resourceSubscribe = true))
        launch            <- launched(live)
        mounted           <- host.mount(live, launch)
        (opened, end)     <- rawPair
        p                 <- page
        _                 <- mounted.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(openGate))
        _                 <- opened.ask(0, "ui/initialize", initialize)
        _                 <- opened.notify("ui/notifications/initialized")
        allowed           <- opened.ask(2, "resources/subscribe", uri)
        _                 <- mcp.toolsChanged
        seen              <- ZIO
          .iterate((Chunk.empty[String], 0))((ns, i) => !ns.contains(Notifications.ToolsListChanged) && i < 40) {
            case (_, i) => opened.notified.zipLeft(ZIO.yieldNow).map(ns => (ns, i + 1))
          }
          .map(_._1)
        subscribed = allowed match
          case Message.Result(_, _) => true
          case _                    => false
      yield assertTrue(
        refused == refusal(1, Denial.NotOffered("resources/subscribe")),
        subscribed,
        seen.contains(Notifications.ToolsListChanged),
      )
      end for
    ,
    test("subscribe forwards the uri the view named, including one this server does not have"):
      val uri    = "notes://today"
      val params = Json.Obj("uri" -> Json.Str(uri))
      for
        (gate, _)         <- answering(ConsentOutcome.AllowForSession)
        (closed, _, _, _) <- raw(gate)
        refused           <- closed.ask(1, "resources/subscribe", params)
        (openGate, _)     <- answering(ConsentOutcome.AllowForSession)
        (_, live, _)      <- serving(view, handshake = McpClient.Handshake.Session)
        host              <- AppsHost.make(settings.copy(resourceSubscribe = true))
        launch            <- launched(live)
        mounted           <- host.mount(live, launch)
        (opened, end)     <- rawPair
        p                 <- page
        _                 <- mounted.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(openGate))
        _                 <- opened.ask(0, "ui/initialize", initialize)
        _                 <- opened.notify("ui/notifications/initialized")
        forwarded         <- opened.ask(2, "resources/subscribe", params)
        reached = forwarded match
          case Message.Error(_, RpcError.ResourceNotFound(message)) => message.contains(uri)
          case _                                                    => false
      yield assertTrue(
        refused == refusal(1, Denial.NotOffered("resources/subscribe")),
        reached,
      )
    ,
    test("what this host does not offer is refused in the shape the spec gives each method"):
      val text = Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("hi")))
      for
        (gate, _)    <- answering(ConsentOutcome.AllowForSession)
        (v, _, _, _) <- raw(gate)
        context <- v.ask(1, "ui/update-model-context", Json.Obj("structuredContent" -> Json.Obj("k" -> Json.Num(1))))
        message <- v.ask(2, "ui/message", Json.Obj("role" -> Json.Str("user"), "content" -> text))
        unknown <- v.ask(3, "sampling/createMessage")
      yield assertTrue(
        context == refusal(1, Denial.NotOffered("ui/update-model-context")),
        message == Message.Result(RequestId.Num(2), Outcome.wire(Outcome.Refused)),
        unknown == Message.Error(Some(RequestId.Num(3)), RpcError.methodNotFound("sampling/createMessage")),
      )
      end for
    ,
    test("only an http or https link is opened, and nothing else reaches the page"):
      val urls = Chunk("https://example.test/a", "javascript:alert(1)", "file:///etc/passwd", "ws://example.test")
      for
        (gate, _)    <- answering(ConsentOutcome.Unavailable)
        (v, _, _, p) <- raw(gate)
        replies      <- ZIO.foreach(urls.zipWithIndex)((url, i) =>
          v.ask(i.toLong + 1, "ui/open-link", Json.Obj("url" -> Json.Str(url)))
        )
        opened <- p.links.get
      yield assertTrue(
        replies.map { case Message.Result(_, r) => Outcome.fromWire(r); case _ => Outcome.Refused } ==
          Chunk(Outcome.Done, Outcome.Refused, Outcome.Refused, Outcome.Refused),
        opened.map(_.render) == Chunk("https://example.test/a"),
      )
      end for
    ,
    test("the host says nothing before initialized, then sends the tool input and result exactly once"):
      for
        (gate, _) <- answering(ConsentOutcome.Unavailable)
        (m, _)    <- mounted()
        (v, end)  <- rawPair
        p         <- page
        _         <- m.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(gate))
        _         <- v.ask(0, "ui/initialize", initialize)
        before    <- v.notified
        _         <- v.notify("ui/notifications/initialized")
        _         <- v.notify("ui/notifications/initialized")
        _         <- v.ask(1, "ping")
        after     <- v.notified
      yield assertTrue(
        before.isEmpty,
        after == Chunk("ui/notifications/tool-input", "ui/notifications/tool-result"),
      )
    ,
    test("a view's height reaches the page only within the host's bounds"):
      for
        (gate, _)    <- answering(ConsentOutcome.Unavailable)
        (v, _, _, p) <- raw(gate)
        _            <- ZIO.foreachDiscard(Chunk(5000.0, 10.0, 300.0))(h =>
          v.notify("ui/notifications/size-changed", Json.Obj("height" -> Json.Num(h)))
        )
        _       <- v.ask(1, "ping")
        heights <- p.heights.get
      yield assertTrue(heights == Chunk(720.0, 96.0, 300.0))
    ,
    test("after the relay reports a navigation, the mount ends and nothing more is heard"):
      for
        (gate, asked)         <- answering(ConsentOutcome.AllowForSession)
        (v, served, count, _) <- raw(gate)
        _                     <- v.notify(SandboxMessage.Navigated.method)
        ending                <- served.ending
        _   <- v.port.send(Message.Request(RequestId.Num(9), "tools/call", Json.Obj("name" -> Json.Str("inc"))))
        log <- audited
        n   <- count.get
        who <- asked.get
      yield assertTrue(
        ending == Ending.Navigated,
        log.exists(e => e.action == Action.Navigated && e.decision == Decision.DroppedAfterNavigate),
        calls(log).isEmpty,
        who.isEmpty,
        n == 0,
      )
    ,
    test("a second ui/initialize is a new document in the frame, so the mount ends and it is never answered"):
      for
        (gate, asked)         <- answering(ConsentOutcome.AllowForSession)
        (v, served, count, _) <- raw(gate)
        _                     <- v.port.send(Message.Request(RequestId.Num(5), "ui/initialize", initialize))
        ending                <- served.ending
        _     <- v.port.send(Message.Request(RequestId.Num(6), "tools/call", Json.Obj("name" -> Json.Str("inc"))))
        log   <- audited
        heard <- v.heard.get
        n     <- count.get
        who   <- asked.get
      yield assertTrue(
        ending == Ending.Navigated,
        log.exists(e => e.action == Action.Navigated && e.decision == Decision.DroppedAfterNavigate),
        !heard.exists {
          case Message.Result(RequestId.Num(5 | 6), _) | Message.Error(Some(RequestId.Num(5 | 6)), _) => true
          case _                                                                                      => false
        },
        calls(log).isEmpty,
        who.isEmpty,
        n == 0,
      )
    ,
    test("teardown asks the view first, and ends when it answers"):
      for
        (gate, _)    <- answering(ConsentOutcome.Unavailable)
        saved        <- Promise.make[Nothing, Unit]
        (m, _)       <- mounted()
        (view, back) <- ViewPort.pair
        p            <- page
        served       <- m.serve(back, p).provideSome[Scope & Audit](ZLayer.succeed(gate))
        bridge       <- AppBridge.connect(
          CounterHost.shed,
          view,
          AppBridge.Settings(Implementation("counter-view", "1"), onTeardown = saved.succeed(()).unit),
        )
        // The host sends the run once it has heard initialized, which is when it may ask the view anything.
        _      <- bridge.run.collect { case Run.Returned(_, _) => () }.runHead
        ending <- served.teardown("the user closed it")
        done   <- saved.isDone
        log    <- audited
      yield assertTrue(
        ending == Ending.TornDown("the user closed it"),
        done,
        log.exists(_.decision == Decision.TornDown("the user closed it")),
      )
    ,
    test("while a call waits on the user, the view is still heard, and its next call is asked too"):
      for
        held  <- Promise.make[Nothing, ConsentOutcome]
        asked <- Queue.unbounded[ConsentRequest]
        gate = new ConsentGate:
          def decide(request: ConsentRequest): UIO[ConsentOutcome] = asked.offer(request) *> held.await
        (v, _, count, _) <- raw(gate)
        first            <- v.ask(1, "tools/call", Json.Obj("name" -> Json.Str("inc"))).fork
        one              <- asked.take
        pong             <- v.ask(2, "ping")
        second           <- v.ask(3, "tools/call", Json.Obj("name" -> Json.Str("inc"))).fork
        two              <- asked.take
        _                <- held.succeed(ConsentOutcome.AllowOnce)
        a                <- first.join
        b                <- second.join
        n                <- count.get
      yield assertTrue(
        one.tool == ToolName("inc"),
        two.tool == ToolName("inc"),
        pong == Message.Result(RequestId.Num(2), Json.Obj()),
        a match
          case Message.Result(RequestId.Num(1), _) => true
          case _                                   => false
        ,
        b match
          case Message.Result(RequestId.Num(3), _) => true
          case _                                   => false
        ,
        n == 2,
      )
    ,
    test("a call still waiting when the mount ends is interrupted, and never answered"):
      for
        asked       <- Queue.unbounded[ConsentRequest]
        interrupted <- Promise.make[Nothing, Unit]
        gate = new ConsentGate:
          def decide(request: ConsentRequest): UIO[ConsentOutcome] =
            asked.offer(request) *> ZIO.never.onInterrupt(interrupted.succeed(()))
        (v, served, count, _) <- raw(gate)
        _     <- v.port.send(Message.Request(RequestId.Num(1), "tools/call", Json.Obj("name" -> Json.Str("inc"))))
        _     <- asked.take
        _     <- v.notify(SandboxMessage.Navigated.method)
        _     <- served.ending
        _     <- interrupted.await
        heard <- v.heard.get
        n     <- count.get
      yield assertTrue(
        !heard.exists {
          case Message.Result(RequestId.Num(1), _) | Message.Error(Some(RequestId.Num(1)), _) => true
          case _                                                                              => false
        },
        n == 0,
      )
    ,
    test("a mount whose scope closes has ended as its port closed, and asking it to go waits on nothing"):
      for
        (gate, _) <- answering(ConsentOutcome.Unavailable)
        (m, _)    <- mounted()
        (_, end)  <- rawPair
        p         <- page
        served    <- ZIO.scoped(m.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(gate)))
        ending    <- served.ending
        leaving   <- served.teardown("too late")
      yield assertTrue(ending == Ending.PortClosed, leaving == Ending.PortClosed)
    ,
    test("a view torn down before it initialized is sent nothing, and ends at once"):
      for
        (gate, _) <- answering(ConsentOutcome.Unavailable)
        (m, _)    <- mounted()
        (v, end)  <- rawPair
        p         <- page
        served    <- m.serve(end, p).provideSome[Scope & Audit](ZLayer.succeed(gate))
        _         <- v.ask(0, "ui/initialize", initialize)
        ending    <- served.teardown("the page closed it")
        heard     <- v.heard.get
        log       <- audited
      yield assertTrue(
        ending == Ending.TornDown("the page closed it"),
        !heard.exists {
          case Message.Request(_, "ui/resource-teardown", _) => true
          case _                                             => false
        },
        log.exists(_.decision == Decision.TornDown("the page closed it")),
      )
    ,
    test("a view that asks to go but never answers teardown is torn down when the host's patience runs out"):
      for
        (gate, _)         <- answering(ConsentOutcome.Unavailable)
        (v, served, _, _) <- raw(gate)
        _                 <- v.notify("ui/notifications/request-teardown")
        asked             <- v.requests.take
        _                 <- TestClock.adjust(settings.teardownWait)
        ending            <- served.ending
      yield assertTrue(asked.method == "ui/resource-teardown", ending == Ending.TornDown("the view asked to close")),
  )

  private val boundsSuite = suite("bounds")(
    test("a clamped height is within the bounds, and a height already within them is kept"):
      check(Gen.double(-1e7, 1e7)) { h =>
        val b = HeightBounds.default
        val c = b.clamp(h)
        assertTrue(c >= b.min, c <= b.max, (h < b.min || h > b.max) || c == h)
      }
    ,
    test("a log burst beyond the budget is dropped, and the budget refills with time"):
      val budget = LogBudget(5, 1.minute)
      val later  = ZStream.fromZIO(TestClock.adjust(1.minute)).drain
      budget
        .enforce(ZStream.range(0, 50) ++ later ++ ZStream.range(50, 100))
        .runCollect
        .map(out => assertTrue(out == Chunk.range(0, 5) ++ Chunk.range(50, 55)))
    ,
    test("no budget lets no log line through"):
      LogBudget(0, 1.minute).enforce(ZStream.range(0, 10)).runCollect.map(out => assertTrue(out.isEmpty)),
  )

  def spec = suite("AppsHost")(mountSuite, serveSuite, boundsSuite).provideSome[Scope](
    HashPins.inMemory,
    Audit.layer(),
  ) @@ TestAspect.timeout(60.seconds)
end AppsHostSpec
