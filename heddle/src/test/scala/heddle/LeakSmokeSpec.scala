package heddle

import zio.*
import zio.test.*

object LeakSmokeSpec extends ZIOSpecDefault:
  def spec =
    suite("leak smoke")(
      test("heap, established TCP, and platform threads return after a handful of requests"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          for
            _     <- Client.get(s"$base/health").repeatN(1)
            _     <- ZIO.succeed(Resources.gc())
            heap0 <- ZIO.succeed(Resources.heapUsed)
            tcp0  <- ZIO.succeed(Resources.establishedTcp)
            pt0   <- ZIO.succeed(Resources.platformThreads)
            _     <- ZIO.foreachDiscard(0 until 8)(_ => Client.get(s"$base/health"))
            _     <- ZIO.succeed(Resources.gc())
            heap1 <- ZIO.succeed(Resources.heapUsed)
            tcp1  <- ZIO.succeed(Resources.establishedTcp)
            pt1   <- ZIO.succeed(Resources.platformThreads)
          yield
            val heapOk = heap1 <= heap0 + 16L * 1024 * 1024
            val tcpOk  = (tcp0, tcp1) match
              case (Some(a), Some(b)) => b <= a + 2
              case _                  => true
            val ptOk = pt1 <= pt0 + 8
            assertTrue(heapOk, tcpOk, ptOk)
        }
    ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.seconds)
end LeakSmokeSpec
