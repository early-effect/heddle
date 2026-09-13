package heddle.brotli

import zio.*
import zio.test.*

object DistanceSpec extends ZIOSpecDefault:
  def spec =
    suite("Distance")(
      test("encode then decode is an inverse"):
        check(Gen.int(1, 1 << 15)) { d =>
          val e = Distance.encode(d)
          assertTrue(Distance.decode(e.symbol, e.extra) == d)
        }
      ,
      test("distance 1 is symbol 16 extra 0"):
        val e = Distance.encode(1)
        assertTrue(e.symbol == 16, e.extraBits == 1, e.extra == 0, Distance.decode(16, 0) == 1),
    ) @@ TestAspect.timeout(30.seconds)
end DistanceSpec
