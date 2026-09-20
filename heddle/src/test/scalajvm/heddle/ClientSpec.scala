package heddle

import BytesLength.*
import zio.*
import zio.test.*

object ClientSpec extends ZIOSpecDefault:
  def spec =
    val live = suite("Client")(
      test("named defaults"):
        val c = Client.Config.default
        assertTrue(
          c.maxConnectionsPerHost == 10,
          c.maxIdlePerHost == 10,
          c.connectTimeout == 10.seconds,
          c.idleTimeout == 60.seconds,
          c.poolIdleTimeout == 60.seconds,
          c.maxHeaderBytes == 64.K,
          c.maxBodyBytes == 10.M,
        )
      ,
      test("get does not add Accept-Encoding"):
        val seen   = java.util.concurrent.atomic.AtomicReference[Option[String]](None)
        val routes = Routes(
          Method.GET / "h" -> Handler { (req: Request) =>
            seen.set(req.header("Accept-Encoding"))
            ZIO.succeed(Response.text("ok"))
          }
        )
        LiveServer(routes) { base =>
          Client.get(s"$base/h").map { res =>
            assertTrue(res.body.asString == "ok", seen.get().isEmpty)
          }
        }
      ,
      test("get sends Accept-Encoding only when the caller set it"):
        val seen   = java.util.concurrent.atomic.AtomicReference[Option[String]](None)
        val routes = Routes(
          Method.GET / "h" -> Handler { (req: Request) =>
            seen.set(req.header("Accept-Encoding"))
            ZIO.succeed(Response.text("ok"))
          }
        )
        LiveServer(routes) { base =>
          Client
            .request(Method.GET, s"$base/h", headers = Headers.empty.add("Accept-Encoding", "gzip"))
            .map(_ => assertTrue(seen.get().contains("gzip")))
        }
      ,
      test("connectTimeout fails a blackhole connect"):
        Client
          .get("http://192.0.2.1:1", Client.Config.default.copy(connectTimeout = 80.millis))
          .either
          .map(e => assertTrue(e.isLeft))
      @@ TestAspect.timeout(3.seconds),
      test("pool exhausted when maxConnectionsPerHost is 1 and two stay in flight"):
        val cfg = Client.Config.default.copy(maxConnectionsPerHost = 1, maxIdlePerHost = 0)
        Promise.make[Nothing, Unit].flatMap { gate =>
          val routes =
            Routes(Method.GET / "hold" -> handler(gate.await.as(Response.text("ok"))))
          ZIO.scoped {
            Server.install(routes, LiveServer.local).flatMap { server =>
              server.port.flatMap { port =>
                val url = s"http://127.0.0.1:$port/hold"
                (for
                  a <- Client.batched(Request.get(url)).fork
                  _ <- ZIO.sleep(80.millis)
                  b <- Client.batched(Request.get(url)).either
                  _ <- gate.succeed(())
                  _ <- a.join
                yield assertTrue(b.isLeft)).provide(ZLayer.succeed(cfg) >>> Client.layer)
              }
            }
          }
        },
    )
    live @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(10.seconds)
  end spec
end ClientSpec
