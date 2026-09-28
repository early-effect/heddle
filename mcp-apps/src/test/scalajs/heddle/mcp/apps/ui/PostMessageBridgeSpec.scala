package heddle.mcp.apps.ui

import ascent.dom
import heddle.mcp.apps.Count
import heddle.mcp.client.McpError
import heddle.mcp.protocol.{Implementation, Message, RequestId}
import scala.scalajs.js
import zio.*
import zio.json.ast.Json
import zio.test.*

object PostMessageBridgeSpec extends ZIOSpecDefault:
  import CounterHost.*

  /** A view and a host on the two ends of one real `MessageChannel`: every message crosses as a structured clone. */
  private val channel: URIO[Scope, (ViewPort, ViewPort)] =
    ZIO.acquireRelease(ZIO.succeed(dom.MessageChannel()))(c => ZIO.succeed { c.port1.close(); c.port2.close() }).map {
      c =>
        (PostMessageBridge.over(PostTarget.port(c.port1)), PostMessageBridge.over(PostTarget.port(c.port2)))
    }

  def spec = suite("PostMessageBridge")(
    test("a message survives a real MessageChannel, id and params intact"):
      ZIO.scoped {
        channel.flatMap { (view, host) =>
          val ping = Message.Request(RequestId.Num(7), "ping", Json.Obj("n" -> Json.Num(1)))
          host.receive.runHead.fork
            .flatMap(got => view.send(ping) *> got.join)
            .map(got => assertTrue(got.contains(ping)))
        }
      }
    ,
    test("the whole bridge runs over postMessage: handshake, a typed call, and the launch tool's run"):
      ZIO.scoped {
        for
          up           <- upstream
          (view, back) <- channel
          h            <- host(back, up)
          bridge       <- AppBridge.connect(shed, view, AppBridge.Settings(Implementation("counter-view", "1")))
          count        <- bridge.call(_.inc)(())
          _            <- h.notify(HostNotification.ToolInput(Json.Obj()))
          called       <- bridge.run.collect { case r @ Run.Called(_) => r }.runHead
        yield assertTrue(bridge.host.hostInfo == hostInfo, count == Count(1), called.contains(Run.Called(())))
      }
    ,
    test("anything that is not JSON-RPC is dropped, and the next message still arrives"):
      ZIO.scoped {
        ZIO
          .acquireRelease(ZIO.succeed(dom.MessageChannel()))(c => ZIO.succeed { c.port1.close(); c.port2.close() })
          .flatMap { c =>
            val host = PostMessageBridge.over(PostTarget.port(c.port2))
            val ping = Message.Request(RequestId.Num(1), "ping", Json.Obj())
            for
              first <- host.receive.runHead.fork
              _     <- ZIO.succeed(c.port1.postMessage("not a message"))
              _     <- ZIO.succeed(c.port1.postMessage(js.Dictionary("jsonrpc" -> "1.0")))
              _     <- PostMessageBridge.over(PostTarget.port(c.port1)).send(ping)
              got   <- first.join
            yield assertTrue(got.contains(ping))
          }
      }
    ,
    test("a value the browser cannot clone fails the send as a pipe failure"):
      ZIO.scoped {
        ZIO
          .acquireRelease(ZIO.succeed(dom.MessageChannel()))(c => ZIO.succeed { c.port1.close(); c.port2.close() })
          .flatMap { c =>
            val fn: js.Function0[Unit] = () => ()
            val refusing               = new PostTarget:
              def post(message: js.Any): Unit                 = c.port1.postMessage(fn)
              def listen(deliver: js.Any => Unit): () => Unit = () => ()
            PostMessageBridge.over(refusing).send(Message.Notification("ping", Json.Obj())).either.map { r =>
              assertTrue(r.left.exists { case McpError.Pipe(_) => true; case _ => false })
            }
          }
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
end PostMessageBridgeSpec
