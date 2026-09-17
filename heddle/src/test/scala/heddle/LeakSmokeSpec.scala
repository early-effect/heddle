package heddle

import zio.*
import zio.test.*

object LeakSmokeSpec extends ZIOSpecDefault:
  def spec =
    suite("leak smoke")(
      test("established TCP on the server port returns after a handful of requests"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          for
            _    <- Client.get(s"$base/health").repeatN(1)
            _    <- ZIO.succeed(Resources.gc())
            tcp0 <- ZIO.succeed(Resources.establishedTcpOn(port))
            fd0  <- ZIO.succeed(Resources.openFiles)
            _    <- ZIO.foreachDiscard(0 until 8)(_ => Client.get(s"$base/health"))
            _    <- ZIO.succeed(Resources.gc())
            tcp1 <- ZIO.succeed(Resources.establishedTcpOn(port))
            fd1  <- ZIO.succeed(Resources.openFiles)
          yield
            val tcpOk = (tcp0, tcp1) match
              case (Some(a), Some(b)) => b <= a + 2
              case _                  => true
            val fdOk = (fd0, fd1) match
              case (Some(a), Some(b)) => b <= a + 32
              case _                  => true
            assertTrue(tcpOk, fdOk)
          end for
        }
    ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.seconds)
end LeakSmokeSpec
