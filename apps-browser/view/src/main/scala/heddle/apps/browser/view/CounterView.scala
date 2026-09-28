package heddle.apps.browser.view

import heddle.apps.browser.counter.{Count, Counter}
import heddle.mcp.apps.ui.*
import heddle.mcp.client.McpCallFailure
import heddle.mcp.protocol.Implementation
import scala.scalajs.js
import zio.*
import zio.stream.ZStream

/** A heddle view: `AppBridge` over `postMessage`, typed from the counter's shed. It shows the count the launch
  * returned, and each click on `#inc` asks the host to call `inc`.
  */
object CounterView extends ZIOAppDefault:
  def run =
    ZIO.scoped(
      for
        bridge <- AppBridge.connect(
          Counter.shed,
          PostMessageBridge.toParent,
          AppBridge.Settings(Implementation("counter-view", "1")),
        )
        count = BrowserDocument.createElement("p")
        press = BrowserDocument.createElement("button")
        _ <- ZIO.succeed {
          count.setAttribute("id", "count")
          count.textContent = "…"
          press.setAttribute("id", "inc")
          press.textContent = "+1"
          val _ = BrowserDocument.body.appendChild(count)
          val _ = BrowserDocument.body.appendChild(press)
        }
        show = (c: Count) => ZIO.succeed(count.textContent = c.value.toString)
        _ <- bridge.run.collect { case Run.Returned(_, out) => out }.foreach(show).forkScoped
        _ <- clicks(press).mapZIO(_ => bridge.call(_.inc)(()).either).foreach {
          case Right(c)                        => show(c)
          case Left(McpCallFailure.Session(e)) => ZIO.succeed(count.textContent = s"refused: ${e.message}")
          case Left(other)                     => ZIO.succeed(count.textContent = s"failed: $other")
        }
      yield ()
    )

  /** One element per click; the listener comes off when the stream ends. */
  private def clicks(target: Element): ZStream[Any, Nothing, Unit] =
    ZStream.asyncScoped[Any, Nothing, Unit] { emit =>
      ZIO.acquireRelease(ZIO.succeed {
        val listener: js.Function1[js.Any, Unit] = _ =>
          val _ = emit(ZIO.succeed(Chunk.unit))
        target.addEventListener("click", listener)
        listener
      })(listener => ZIO.succeed(target.removeEventListener("click", listener)))
    }
end CounterView
