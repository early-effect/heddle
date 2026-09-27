package heddle

import heddle.RawServer.After
import zio.*
import zio.test.*

object ClientPoolSpec extends ZIOSpecDefault:
  private val routes = Routes(
    Method.GET / "hang" -> Handler((_: Request) => ZIO.never),
    Method.GET / "ok"   -> Handler.text("ok"),
  )

  private def pool(cfg: Client.Config = Client.Config.default): Layer[ClientError, Client] =
    ZLayer.succeed(cfg.copy(maxConnectionsPerHost = 1)) >>> Client.layer

  private val framed    = (_: String) => "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"
  private val unframed  = (_: String) => "HTTP/1.1 200 OK\r\n\r\nhello"
  private val headReply = (_: String) => "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"

  def spec = suite("Client pool")(
    test("an interrupted exchange returns its connection slot") {
      LiveServer(routes) { base =>
        (for
          hung <- Client.batched(Request.get(s"$base/hang")).timeout(200.millis)
          ok   <- Client.batched(Request.get(s"$base/ok"))
        yield assertTrue(hung.isEmpty, ok.body.text.is(_.some) == "ok")).provideLayer(pool())
      }
    },
    test("a close-delimited body never goes back to the pool") {
      ZIO.scoped {
        RawServer(unframed, After.Close).flatMap { (base, seen) =>
          (for
            a <- Client.batched(Request.get(s"$base/a"))
            b <- Client.batched(Request.get(s"$base/b"))
            n <- seen.accepted.get
          yield assertTrue(a.body.text.is(_.some) == "hello", b.body.text.is(_.some) == "hello", n == 2))
            .provideLayer(pool())
        }
      }
    },
    test("a keep-alive connection is reused") {
      ZIO.scoped {
        RawServer(framed, After.Keep).flatMap { (base, seen) =>
          (for
            _ <- Client.batched(Request.get(s"$base/a")).repeatN(3)
            n <- seen.accepted.get
          yield assertTrue(n == 1)).provideLayer(pool())
        }
      }
    },
    test("a GET on a pooled connection the server dropped is sent again on a new one") {
      ZIO.scoped {
        RawServer(framed, After.Close).flatMap { (base, seen) =>
          (for
            a <- Client.batched(Request.get(s"$base/a"))
            _ <- ZIO.sleep(50.millis)
            b <- Client.batched(Request.get(s"$base/b"))
            n <- seen.accepted.get
          yield assertTrue(a.body.text.is(_.some) == "ok", b.body.text.is(_.some) == "ok", n == 2)).provideLayer(pool())
        }
      }
    },
    test("a POST on a dropped pooled connection fails instead of sending twice") {
      ZIO.scoped {
        RawServer(framed, After.Close).flatMap { (base, _) =>
          (for
            _ <- Client.batched(Request.get(s"$base/a"))
            _ <- ZIO.sleep(50.millis)
            b <- Client.batched(Request.post(s"$base/b", Body.text("x"))).either
          yield assertTrue(b match
            case Left(_: ClientError.Io) => true
            case _                       => false)).provideLayer(pool())
        }
      }
    },
    test("HEAD reads no body even when Content-Length says otherwise") {
      ZIO.scoped {
        RawServer(headReply, After.Keep).flatMap { (base, _) =>
          (for
            a <- Client.batched(Request(Method.HEAD, Url.parse(s"$base/a")))
            b <- Client.batched(Request(Method.HEAD, Url.parse(s"$base/b")))
          yield assertTrue(a.body.isEmpty, b.status == Status.Ok)).provideLayer(pool())
        }
      }
    },
    test("a close-delimited body over maxBodyBytes is BodyTooLarge") {
      ZIO.scoped {
        RawServer(unframed, After.Close).flatMap { (base, _) =>
          Client
            .batched(Request.get(s"$base/a"))
            .either
            .provideLayer(pool(Client.Config.default.copy(maxBodyBytes = 3.B)))
            .map(out =>
              assertTrue(out match
                case Left(ClientError.Protocol(_, HttpError.BodyTooLarge)) => true
                case _                                                     => false)
            )
        }
      }
    },
    test("a Content-Length body larger than one read arrives whole") {
      val big    = "x" * (3 * 1024 * 1024 + 17)
      val routes = Routes(Method.GET / "big" -> Handler.text(big))
      LiveServer(routes) { base =>
        Client
          .batched(Request.get(s"$base/big"))
          .provideLayer(pool())
          .map(res => assertTrue(res.body.text.is(_.some) == big))
      }
    },
    test("a peer that never answers is ReadTimeout after idleTimeout") {
      ZIO.scoped {
        RawServer(_ => "", After.Keep).flatMap { (base, _) =>
          Client
            .batched(Request.get(s"$base/a"))
            .either
            .provideLayer(pool(Client.Config.default.copy(idleTimeout = 300.millis)))
            .map(out =>
              assertTrue(out match
                case Left(_: ClientError.ReadTimeout) => true
                case _                                => false)
            )
        }
      }
    },
    test("an origin-form request with no Host is InvalidTarget") {
      Client
        .batched(Request.get("/nowhere"))
        .either
        .provideLayer(pool())
        .map(out =>
          assertTrue(out match
            case Left(_: ClientError.InvalidTarget) => true
            case _                                  => false)
        )
    },
    test("a refused connection is Connect") {
      ZIO.attemptBlocking(java.net.ServerSocket(0)).flatMap { ss =>
        val port = ss.getLocalPort
        ss.close()
        Client
          .get(s"http://127.0.0.1:$port/")
          .either
          .map(out =>
            assertTrue(out match
              case Left(_: ClientError.Connect) => true
              case _                            => false)
          )
      }
    },
    test("a malformed URL string is InvalidTarget") {
      Client
        .get("http://exam ple.com/")
        .either
        .map(out =>
          assertTrue(out match
            case Left(_: ClientError.InvalidTarget) => true
            case _                                  => false)
        )
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
end ClientPoolSpec
