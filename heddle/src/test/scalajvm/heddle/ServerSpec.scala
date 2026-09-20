package heddle

import BytesLength.*
import java.io.{ByteArrayOutputStream, InputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import zio.*
import zio.stream.ZStream
import zio.test.*

object ServerSpec extends ZIOSpecDefault:
  def spec =
    suite("Server")(
      test("defaultWith provides Config and install binds"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO
          .scoped {
            Server.install(routes).flatMap { server =>
              server.port.flatMap { p =>
                Client.get(s"http://127.0.0.1:$p/health").map { res =>
                  assertTrue(res.status == Status.Ok, res.body.asString == "ok")
                }
              }
            }
          }
          .provide(Server.defaultWith(_.port(0)))
      ,
      test("Client.live batched fetches an absolute URL"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          Client.batched(Request.get(s"$base/health")).provide(Client.live).map { res =>
            assertTrue(res.body.asString == "ok")
          }
        }
      ,
      test("loom server serves a text route over HTTP/1.1"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          Client.get(s"$base/health").map { res =>
            assertTrue(res.status == Status.Ok, res.body.asString == "ok")
          }
        }
      ,
      test("handlers run on a virtual thread"):
        val routes = Routes(
          Method.GET / "vt" -> handler {
            ZIO.succeed(Response.text(Thread.currentThread.isVirtual.toString))
          }
        )
        LiveServer(routes) { base =>
          Client.get(s"$base/vt").map { res =>
            assertTrue(res.body.asString == "true")
          }
        }
      ,
      test("JvmScheduler.Default still binds"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server.install(routes, LiveServer.local, JvmScheduler.Default).flatMap { server =>
            server.port.flatMap { p =>
              Client.get(s"http://127.0.0.1:$p/health").map { res =>
                assertTrue(res.status == Status.Ok, res.body.asString == "ok")
              }
            }
          }
        }
      ,
      test("unknown paths are 404 on the wire"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          Client.get(s"$base/nope").map { res =>
            assertTrue(res.status == Status.NotFound)
          }
        }
      ,
      test("serves concurrent connections"):
        val routes = Routes(Method.GET / "n" -> handler(ZIO.sleep(20.millis).as(Response.text("ok"))))
        LiveServer(routes) { base =>
          ZIO.foreachPar(0 until 32)(_ => Client.get(s"$base/n")).map { rs =>
            assertTrue(rs.size == 32, rs.forall(r => r.status == Status.Ok && r.body.asString == "ok"))
          }
        }
      @@ TestAspect.timeout(5.seconds),
      test("keep-alive reuses one connection for a second request"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                val a = writeGet(sock, "/a")
                val b = writeGet(sock, "/b")
                (a, b)
              finally sock.close()
            }
            .map { (a, b) =>
              assertTrue(a.contains("A"), b.contains("B"))
            }
        }
      ,
      test("unread POST body is drained before a keep-alive GET"):
        val routes = Routes(
          Method.POST / "drop" -> Handler.text("dropped"),
          Method.GET / "a"     -> Handler.text("A"),
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                val post =
                  "POST /drop HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\n\r\nping"
                sock.getOutputStream.write(post.getBytes(StandardCharsets.US_ASCII))
                sock.getOutputStream.flush()
                val dropped = readHttpResponse(sock.getInputStream)
                val a       = writeGet(sock, "/a")
                (dropped, a)
              finally sock.close()
            }
            .map { (dropped, a) =>
              assertTrue(dropped.contains("dropped"), a.contains("A"))
            }
        }
      ,
      test("Connection: close is advertised and the socket is not reused"):
        val routes = Routes(Method.GET / "a" -> Handler.text("A"))
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                val raw = "GET /a HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
                sock.getOutputStream.write(raw.getBytes(StandardCharsets.US_ASCII))
                sock.getOutputStream.flush()
                val first = readHttpResponse(sock.getInputStream)
                sock.getOutputStream.write("GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
                sock.getOutputStream.flush()
                val second =
                  try sock.getInputStream.read()
                  catch case _: java.io.IOException => -1
                (first, second)
              finally sock.close()
              end try
            }
            .map { (first, second) =>
              assertTrue(first.contains("A"), first.contains("Connection: close"), second < 0)
            }
        }
      ,
      test("keep-alive serves swagger html then openapi.json"):
        val spec = OpenApi.from("Ping", "0.0.1", Endpoint.get("health").outText())
        val app  = spec.routes("docs")
        LiveServer(app) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                val html = writeGet(sock, "/docs")
                val json = writeGet(sock, "/docs/openapi.json")
                (html, json)
              finally sock.close()
            }
            .map { (html, json) =>
              assertTrue(html.contains("swagger-ui"), json.contains("\"openapi\""))
            }
        }
      ,
      test("POST echoes the request body"):
        val routes = Routes(
          Method.POST / "echo" -> handler { (req: Request) => ZIO.succeed(Response(Status.Ok).withBody(req.body)) }
        )
        LiveServer(routes) { base =>
          Client.request(Method.POST, s"$base/echo", body = Body.text("ping")).map { res =>
            assertTrue(res.body.asString == "ping")
          }
        }
      ,
      test("rejects a body larger than maxBodyBytes"):
        val routes = Routes(Method.POST / "echo" -> Handler.text("ok"))
        val config = LiveServer.local.copy(maxBodyBytes = 4.B)
        LiveServer(routes, config) { base =>
          Client.request(Method.POST, s"$base/echo", body = Body.text("hello")).map { res =>
            assertTrue(res.status == Status.ContentTooLarge)
          }
        }
      ,
      test("rejects headers larger than maxHeaderBytes"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok"))
        val config = LiveServer.local.copy(maxHeaderBytes = 64.B)
        LiveServer(routes, config) { base =>
          Client.request(Method.GET, s"$base/x", headers = Headers("X-Big", "a" * 200)).map { res =>
            assertTrue(res.status == Status.RequestHeaderFieldsTooLarge)
          }
        }
      ,
      test("install reads Config from the environment"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO
          .scoped {
            Server.install(routes).flatMap { server =>
              server.port.flatMap { port =>
                Client.get(s"http://127.0.0.1:$port/health").map { res =>
                  assertTrue(res.status == Status.Ok, res.body.asString == "ok")
                }
              }
            }
          }
          .provide(ZLayer.succeed(LiveServer.local))
      ,
      test("install fails with BindFailed when the port is out of range"):
        val config = LiveServer.local.copy(port = -1)
        ZIO.scoped(Server.install(Routes.empty, config)).flip.map { case ServerError.BindFailed(host, port, _) =>
          assertTrue(host == config.host, port == -1)
        }
      ,
      test("install fails with BindFailed when the address is in use"):
        val config = LiveServer.local.copy(reuseAddress = false)
        ZIO.scoped {
          for
            server <- Server.install(Routes.empty, config)
            port   <- server.port
            err    <- Server.install(Routes.empty, config.copy(port = port)).flip
          yield err match
            case ServerError.BindFailed(host, bound, _) => assertTrue(host == config.host, bound == port)
        }
      ,
      test("closing the scope unbinds the port"):
        val config = LiveServer.local.copy(reuseAddress = false)
        for
          port <- ZIO.scoped(Server.install(Routes.empty, config).flatMap(_.port))
          _    <- ZIO.scoped(rebind(config, port).unit)
        yield assertTrue(true)
      ,
      test("interrupting serve unbinds the port"):
        val config = LiveServer.local.copy(reuseAddress = false)
        for
          bound <- Promise.make[Nothing, Int]
          fiber <- ZIO
            .scoped(Server.install(Routes.empty, config).flatMap(s => s.port.flatMap(bound.succeed) *> ZIO.never))
            .fork
          port <- bound.await
          _    <- fiber.interrupt
          _    <- ZIO.scoped(rebind(config, port).unit)
        yield assertTrue(true)
      ,
      test("interrupting Server.serve completes quickly"):
        val config = LiveServer.local
        for
          fiber   <- Server.serve(Routes.empty, config).fork
          _       <- ZIO.sleep(100.millis)
          elapsed <- fiber.interrupt.timed.map(_._1)
        yield assertTrue(elapsed.toMillis < 2000)
      @@ TestAspect.timeout(5.seconds),
      test("shutdown unbinds the port"):
        val config = LiveServer.local.copy(reuseAddress = false)
        ZIO.scoped {
          for
            first  <- Server.install(Routes.empty, config)
            port   <- first.port
            _      <- first.shutdown
            second <- rebind(config, port)
            bound  <- second.port
          yield assertTrue(bound == port)
        }
      ,
      test("shutdown waits for an in-flight request up to gracefulShutdownTimeout"):
        val config = LiveServer.local.copy(gracefulShutdownTimeout = 2.seconds)
        for
          started <- Promise.make[Nothing, Unit]
          routes = Routes(
            Method.GET / "slow" -> handler { (_: Request) =>
              started.succeed(()) *> ZIO.sleep(200.millis).as(Response.text("ok"))
            }
          )
          result <- ZIO.scoped {
            for
              server <- Server.install(routes, config)
              port   <- server.port
              client <- ZIO.attemptBlocking {
                val sock = Socket("127.0.0.1", port)
                try writeGet(sock, "/slow")
                finally sock.close()
              }.fork
              _    <- started.await
              _    <- server.shutdown
              wire <- client.join
            yield assertTrue(wire.contains("200"), wire.contains("ok"))
          }
        yield result
        end for
      @@ TestAspect.timeout(5.seconds),
      test("shutdown force-kills a stuck send after gracefulShutdownTimeout"):
        val routes = Routes(
          Method.GET / "hang" -> handler(
            ZIO.succeed(Response(Status.Ok).withBody(Body.stream(ZStream.never)))
          )
        )
        val config = LiveServer.local.copy(gracefulShutdownTimeout = 50.millis)
        ZIO.scoped {
          for
            server  <- Server.install(routes, config)
            port    <- server.port
            client  <- Client.get(s"http://127.0.0.1:$port/hang").fork
            _       <- ZIO.sleep(50.millis)
            elapsed <- server.shutdown.timed.map(_._1)
            _       <- client.interrupt
          yield assertTrue(elapsed.toMillis < 2000)
        }
      @@ TestAspect.timeout(5.seconds),
      test("closing the scope does not hang on an idle keep-alive connection"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        val config = LiveServer.local.copy(reuseAddress = false)
        ZIO
          .scoped {
            for
              server <- Server.install(routes, config)
              port   <- server.port
              _      <- ZIO.attemptBlocking {
                val sock = java.net.Socket("127.0.0.1", port)
                val raw  = "GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n"
                sock.getOutputStream.write(raw.getBytes)
                sock.getOutputStream.flush()
                sock.getInputStream.read()
              }
            yield ()
          }
          .as(assertTrue(true)),
    ) @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock @@ TestAspect.sequential

  /** `reuseAddress = false` plus a sibling `bind(0)` can steal the just-freed port; retry the rebind. */
  private def rebind(config: Server.Config, port: Int) =
    Server.install(Routes.empty, config.copy(port = port)).retry(Schedule.spaced(20.millis) && Schedule.recurs(25))

  private def writeGet(sock: Socket, path: String): String =
    val raw = s"GET $path HTTP/1.1\r\nHost: localhost\r\n\r\n"
    sock.getOutputStream.write(raw.getBytes(StandardCharsets.US_ASCII))
    sock.getOutputStream.flush()
    readHttpResponse(sock.getInputStream)

  private def readHttpResponse(in: InputStream): String =
    val headers = ByteArrayOutputStream()
    var state   = 0
    var done    = false
    while !done do
      val b = in.read()
      if b < 0 then done = true
      else
        headers.write(b)
        state = (state, b) match
          case (0, '\r') => 1
          case (1, '\n') => 2
          case (2, '\r') => 3
          case (3, '\n') =>
            done = true
            0
          case (2, _) => 0
          case _      => 0
      end if
    end while
    val head = String(headers.toByteArray, StandardCharsets.US_ASCII)
    val len  =
      head
        .split("\r\n")
        .find(_.toLowerCase.startsWith("content-length:"))
        .flatMap(_.split(":", 2).lift(1).map(_.trim.toIntOption))
        .flatten
        .getOrElse(0)
    val body = if len > 0 then in.readNBytes(len) else Array.emptyByteArray
    head + String(body, StandardCharsets.UTF_8)
  end readHttpResponse
end ServerSpec
