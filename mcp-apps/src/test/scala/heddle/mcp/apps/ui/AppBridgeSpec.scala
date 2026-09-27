package heddle.mcp.apps.ui

import heddle.*
import heddle.mcp.apps.Count

import heddle.mcp.client.{McpCallFailure, McpError}
import heddle.mcp.protocol.*
import zio.*
import zio.json.ast.Json
import zio.test.*

object AppBridgeSpec extends ZIOSpecDefault:
  import CounterHost.*

  /** A connected view and its host, with the upstream server behind the host. */
  private def connected(onTeardown: UIO[Unit] = ZIO.unit, timeout: Duration = 30.seconds) =
    for
      up           <- upstream
      (view, back) <- ViewPort.pair
      h            <- host(back, up)
      bridge       <- AppBridge.connect(
        shed,
        view,
        AppBridge.Settings(Implementation("counter-view", "1"), requestTimeout = timeout, onTeardown = onTeardown),
      )
    yield (bridge, h, up)

  /** The first state the stream reaches that `pf` accepts. The streams never end, so this waits until one arrives. */
  private def reaches[A](stream: zio.stream.ZStream[Any, Nothing, A])(pf: PartialFunction[A, Boolean]): UIO[Option[A]] =
    stream.filter(a => pf.applyOrElse(a, (_: A) => false)).runHead

  def spec = suite("AppBridge")(
    test("the handshake answers with the host and its context, then the view says it is initialized"):
      ZIO.scoped {
        connected().flatMap { (bridge, h, _) =>
          for
            ctx  <- bridge.context.runHead
            seen <- h.seen.get.repeatUntil(_.exists {
              case Message.Notification(m, _) => m == ViewNotification.Initialized.method; case _ => false
            })
          yield assertTrue(
            bridge.host.hostInfo == hostInfo,
            ctx.contains(start),
            seen.collect { case Message.Request(_, m, _) => m }.headOption.contains("ui/initialize"),
          )
        }
      }
    ,
    test("a typed call through the host reaches the server and comes back typed"):
      ZIO.scoped {
        connected().flatMap { (bridge, _, _) =>
          (bridge.call(_.inc)(()) <*> bridge.call(_.inc)(())).map(counts => assertTrue(counts == (Count(1), Count(2))))
        }
      }
    ,
    test("a call the host refuses is a session failure naming the host's reason"):
      ZIO.scoped {
        connected().flatMap((bridge, _, _) => bridge.call(_.reset)(()).either).map { r =>
          assertTrue(
            r == Left(McpCallFailure.Session(McpError.Rpc(RpcError.InvalidParams("reset is not linked to this view"))))
          )
        }
      }
    ,
    test("the launch tool's run goes from pending to called to returned, typed from the shed"):
      ZIO.scoped {
        connected().flatMap { (bridge, h, up) =>
          for
            _       <- h.notify(HostNotification.ToolInputPartial(Json.Obj("draft" -> Json.Bool(true))))
            pending <- reaches(bridge.run) { case Run.Pending(Some(_)) => true }
            _       <- h.notify(HostNotification.ToolInput(Json.Obj()))
            called  <- reaches(bridge.run) { case Run.Called(_) => true }
            result  <- up.callTool(ToolName("show_counter"), Json.Obj())
            _       <- h.notify(HostNotification.ToolResult(result))
            done    <- reaches(bridge.run) { case Run.Returned(_, _) => true }
          yield assertTrue(
            pending.contains(Run.Pending(Some(Json.Obj("draft" -> Json.Bool(true))))),
            called.contains(Run.Called(())),
            done.contains(Run.Returned((), Count(0))),
          )
        }
      }
    ,
    test("a result before any input is unreadable, and a cancel says why"):
      ZIO.scoped {
        connected().flatMap { (bridge, h, up) =>
          for
            result <- up.callTool(ToolName("show_counter"), Json.Obj())
            _      <- h.notify(HostNotification.ToolResult(result))
            early  <- reaches(bridge.run) { case Run.Unreadable(_) => true }
            _      <- h.notify(HostNotification.ToolCancelled(Some("the user closed it")))
            gone   <- reaches(bridge.run) { case Run.Cancelled(_) => true }
          yield assertTrue(
            early.contains(Run.Unreadable("a tool result arrived before its input")),
            gone.contains(Run.Cancelled(Some("the user closed it"))),
          )
        }
      }
    ,
    test("a context patch replaces only what it names"):
      ZIO.scoped {
        connected().flatMap { (bridge, h, _) =>
          h.notify(HostNotification.HostContextChanged(HostContext(theme = Some(Theme.Dark)))) *>
            reaches(bridge.context) { case c => c.theme.contains(Theme.Dark) }.map { ctx =>
              assertTrue(ctx.flatMap(_.displayMode).contains(DisplayMode.Inline))
            }
        }
      }
    ,
    test("teardown runs the view's hook before the view answers; an unknown host request is refused"):
      ZIO.scoped {
        for
          saved     <- Promise.make[Nothing, Unit]
          (_, h, _) <- connected(onTeardown = saved.succeed(()).unit)
          _         <- h.port.send(HostRequest.ResourceTeardown(Some("closing")).message(RequestId.Num(99)))
          _         <- h.port.send(Message.Request(RequestId.Num(100), "ui/eval", Json.Obj()))
          seen      <- h.seen.get.repeatUntil(_.count {
            case Message.Result(RequestId.Num(99), _)       => true
            case Message.Error(Some(RequestId.Num(100)), _) => true
            case _                                          => false
          } == 2)
          hooked <- saved.isDone
        yield assertTrue(
          hooked,
          seen.exists {
            case Message.Error(Some(RequestId.Num(100)), RpcError.MethodNotFound(_)) => true; case _ => false
          },
        )
      }
    ,
    test("the host's answers are typed: a refused link, and the display mode it chose"):
      ZIO.scoped {
        connected().flatMap { (bridge, _, _) =>
          (bridge.openLink("https://example.test/") <*> bridge.requestDisplayMode(DisplayMode.Pip)).map {
            (link, mode) =>
              assertTrue(link == Outcome.Refused, mode == DisplayMode.Fullscreen)
          }
        }
      }
    ,
    test("a request the host never answers times out on the clock, naming its method"):
      ZIO.scoped {
        connected(timeout = 5.seconds).flatMap { (bridge, _, _) =>
          for
            fiber <- bridge.updateModelContext(Chunk(ContentBlock.Text("count is 2"))).either.fork
            _     <- TestClock.adjust(6.seconds)
            out   <- fiber.join
          yield assertTrue(out == Left(McpError.TimedOut("ui/update-model-context")))
        }
      },
  ) @@ TestAspect.timeout(60.seconds)
end AppBridgeSpec
