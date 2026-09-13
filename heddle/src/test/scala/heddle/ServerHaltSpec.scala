package heddle

import zio.*
import zio.test.*

/** Halt's idle poll uses `ZIO.sleep`. Suites must not `.provide` in the test body (that drops Clock). */
object ServerHaltSpec extends ZIOSpecDefault:

  private val ping = Routes(Method.GET / "ping" -> Handler.text("pong"))

  private val sse = Routes(
    Method.GET / "sse" -> Handler { (_: Request) =>
      heddle.sse.Sse.session[Any] { w =>
        w.send(heddle.sse.ServerSentEvent("x", Some("x"))) *> ZIO.never
      }
    }
  )

  private def roundTrip: ZIO[Server.Config, Throwable, String] =
    ZIO.scoped(
      for
        server <- Server.install(ping).mapError(e => RuntimeException(e.message))
        port   <- server.port
        res    <- Client.get(s"http://127.0.0.1:$port/ping")
        body   <- res.body.utf8
      yield body
    )

  def spec =
    suite("server halt")(
      suite("withLiveClock")(
        test("scoped install serves then closes"):
          roundTrip.map(body => assertTrue(body == "pong"))
        ,
        test("a second scoped install works after the first halt"):
          (roundTrip *> roundTrip).map(body => assertTrue(body == "pong"))
        ,
        test("open SSE shutdown completes without TestClock.adjust"):
          val config = Server.Config.default.copy(port = 0, gracefulShutdownTimeout = 50.millis)
          ZIO.scoped(
            for
              server  <- Server.install(sse, config).mapError(e => RuntimeException(e.message))
              port    <- server.port
              _       <- Client.get(s"http://127.0.0.1:$port/sse").fork
              _       <- ZIO.sleep(50.millis)
              elapsed <- server.shutdown.timed.map(_._1)
            yield assertTrue(elapsed.toMillis < 2000)
          )
        ,
        test("open SSE scope close completes without an explicit shutdown"):
          val config = Server.Config.default.copy(port = 0, gracefulShutdownTimeout = 50.millis)
          ZIO
            .scoped(
              for
                server <- Server.install(sse, config).mapError(e => RuntimeException(e.message))
                port   <- server.port
                _      <- Client.get(s"http://127.0.0.1:$port/sse").fork
                _      <- ZIO.sleep(50.millis)
              yield assertCompletes
            )
            .timed
            .map((elapsed, _) => assertTrue(elapsed.toMillis < 2000)),
      ).provideSomeShared[Scope](Server.defaultWith(_.port(0))) @@ TestAspect.withLiveClock @@ TestAspect.timeout(
        5.seconds
      ),
      suite("TestClock")(
        test("open SSE shutdown joins only after TestClock.adjust of the grace period"):
          val config = Server.Config.default.copy(port = 0, gracefulShutdownTimeout = 1.second)
          ZIO.scoped(
            for
              server <- Server.install(sse, config).mapError(e => RuntimeException(e.message))
              port   <- server.port
              sock   <- ZIO.attemptBlocking {
                val s = java.net.Socket("127.0.0.1", port)
                val q = "GET /sse HTTP/1.1\r\nHost: localhost\r\n\r\n"
                s.getOutputStream.write(q.getBytes)
                s.getOutputStream.flush()
                s.getInputStream.read()
                s
              }
              shutting <- server.shutdown.fork
              _        <- TestClock.adjust(10.millis)
              early    <- shutting.poll
              _        <- TestClock.adjust(2.seconds)
              _        <- shutting.join
              _        <- ZIO.succeed(sock.close())
            yield assertTrue(early.isEmpty)
          )
      ) @@ TestAspect.timeout(5.seconds),
    ) @@ TestAspect.sequential
end ServerHaltSpec
