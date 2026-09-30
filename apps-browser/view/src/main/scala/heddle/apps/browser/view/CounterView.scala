package heddle.apps.browser.view

import ascent.dom
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
        body <- ZIO.fromOption(dom.document.body).orDieWith(_ => IllegalStateException("the view has no body"))
        (count, press) <- ZIO.succeed {
          val count = dom.document.createElement(dom.HtmlTag.p)
          val press = dom.document.createElement(dom.HtmlTag.button)
          count.setAttribute("id", "count")
          count.textContent = Some("…")
          press.setAttribute("id", "inc")
          press.textContent = Some("+1")
          val _ = body.appendChild(count)
          val _ = body.appendChild(press)
          (count, press)
        }
        show = (c: Count) => ZIO.succeed(count.textContent = Some(c.value.toString))
        _ <- bridge.run.collect { case Run.Returned(_, out) => out }.foreach(show).forkScoped
        _ <- clicks(press).mapZIO(_ => bridge.call(_.inc)(()).either).foreach {
          case Right(c)                        => show(c)
          case Left(McpCallFailure.Session(e)) => ZIO.succeed(count.textContent = Some(s"refused: ${e.message}"))
          case Left(other)                     => ZIO.succeed(count.textContent = Some(s"failed: $other"))
        }
      yield ()
    )

  /** One element per click; the listener comes off when the stream ends. */
  private def clicks(target: dom.EventTarget): ZStream[Any, Nothing, Unit] =
    ZStream.asyncScoped[Any, Nothing, Unit] { emit =>
      ZIO.acquireRelease(ZIO.succeed {
        val listener: js.Function1[dom.Event, Unit] = _ =>
          val _ = emit(ZIO.succeed(Chunk.unit))
        target.addEventListener("click", listener)
        listener
      })(listener => ZIO.succeed(target.removeEventListener("click", listener)))
    }
end CounterView
