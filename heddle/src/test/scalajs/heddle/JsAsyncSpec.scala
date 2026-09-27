package heddle

import heddle.internal.node.JsAsync
import scala.scalajs.js
import zio.*
import zio.test.*

object JsAsyncSpec extends ZIOSpecDefault:
  def spec =
    suite("JsAsync")(
      test("a resolved Promise succeeds"):
        JsAsync.fromPromise(js.Promise.resolve[Int](7)).map(n => assertTrue(n == 7))
      ,
      test("a rejected Promise fails with its message"):
        val p = js.Promise.reject(js.Error("nope")).asInstanceOf[js.Promise[Int]]
        JsAsync.fromPromise(p).flip.map(e => assertTrue(e.getMessage.contains("nope")))
      ,
      test("each run of the effect starts the promise again"):
        val started = java.util.concurrent.atomic.AtomicInteger(0)
        val next    = JsAsync.fromPromise(js.Promise.resolve[Int](started.getAndIncrement()))
        next.repeatN(2).map(last => assertTrue(last == 2, started.get == 3)),
    ) @@ TestAspect.timeout(5.seconds)
end JsAsyncSpec
