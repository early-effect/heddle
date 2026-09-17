package heddle

import zio.*
import zio.test.*

/** Prove Clock/fork before touching product tests. Connection fibers are `forkDaemon` from the accept loop
  * (`forkScoped`). They inherit the installer Clock: live under `withLiveClock`, TestClock otherwise. Do not inject
  * `Clock.live`.
  */
object ClockForkSpec extends ZIOSpecDefault:
  def spec =
    val live = suite("installer Clock is live")(
      test("forkDaemon sleep completes"):
        for
          done <- Promise.make[Nothing, Unit]
          _    <- (ZIO.sleep(30.millis) *> done.succeed(())).forkDaemon
          _    <- done.await
        yield assertCompletes
      ,
      test("forkScoped then forkDaemon sleep completes"):
        ZIO.scoped {
          for
            done <- Promise.make[Nothing, Unit]
            _    <- ((ZIO.sleep(30.millis) *> done.succeed(())).forkDaemon *> ZIO.never).forkScoped
            _    <- done.await
          yield assertCompletes
        },
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.seconds)

    val frozen = suite("installer Clock is TestClock")(
      test("forkDaemon sleep waits for TestClock.adjust"):
        for
          done  <- Promise.make[Nothing, Unit]
          _     <- (ZIO.sleep(1.second) *> done.succeed(())).forkDaemon
          _     <- TestClock.adjust(10.millis)
          early <- done.poll
          _     <- TestClock.adjust(2.seconds)
          _     <- done.await
        yield assertTrue(early.isEmpty)
    ) @@ TestAspect.timeout(5.seconds)

    suite("connection fork clock")(live, frozen)
  end spec
end ClockForkSpec
