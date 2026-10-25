package heddle

import heddle.http.header.{HttpDate, OriginDate}
import java.time.Instant
import zio.*
import zio.test.*

object OriginDateSpec extends ZIOSpecDefault:
  def spec =
    suite("OriginDate")(
      test("stamp tracks TestClock and advances on the second boundary") {
        ZIO.scoped {
          for
            originDate <- OriginDate.make
            t0         <- originDate.get.map(_.value)
            t1         <- TestClock.adjust(1.second) *> originDate.get.map(_.value)
            t1Again    <- originDate.get.map(_.value)
          yield assertTrue(
            t0 != t1,
            t1 == t1Again,
            t0.length == 29,
            HttpDate.parse(t0).contains(Instant.EPOCH),
            HttpDate.parse(t1).contains(Instant.EPOCH.plusSeconds(1)),
          )
        }
      },
      test("render uses a zero-padded day") {
        val t = Instant.parse("2008-06-03T11:05:30Z")
        assertTrue(HttpDate.render(t) == "Tue, 03 Jun 2008 11:05:30 GMT")
      },
    ) @@ TestAspect.timeout(5.seconds)
