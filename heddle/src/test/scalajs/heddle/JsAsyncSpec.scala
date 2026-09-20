package heddle

import scala.language.implicitConversions
import heddle.internal.node.JsAsync.given
import scala.scalajs.js
import zio.*
import zio.test.*

object JsAsyncSpec extends ZIOSpecDefault:
  def spec =
    suite("JsAsync")(
      test("Conversion turns a resolved Promise into Task"):
        val p: js.Promise[Int] = js.Promise.resolve[Int](7)
        val z: Task[Int]       = p
        z.map(n => assertTrue(n == 7))
      ,
      test("rejected Promise fails the Task"):
        val p: js.Promise[Int] = js.Promise.reject(js.Error("nope")).asInstanceOf[js.Promise[Int]]
        val z: Task[Int]       = p
        z.flip.map(e => assertTrue(e.getMessage.contains("nope"))),
    ) @@ TestAspect.timeout(5.seconds)
end JsAsyncSpec
