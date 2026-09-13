package heddle

import java.io.IOException
import java.nio.charset.StandardCharsets
import heddle.internal.engine.Http1
import heddle.sse.{ServerSentEvent, Sse, SseCodec}
import zio.*
import zio.stream.ZStream
import zio.test.*

object Http1Spec extends ZIOSpecDefault:
  def spec =
    suite("Http1")(
      test("serveConnection writes 200 for a matching GET"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        runWire(routes, get("/health")).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 200"), wire.endsWith("ok"))
        }
      ,
      test("serveConnection writes 404 for an unknown path"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        runWire(routes, get("/nope")).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 404"))
        }
      ,
      test("serveConnection writes 400 for a malformed request line"):
        runWire(Routes.empty, "GET\r\n\r\n").map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 400"))
        }
      ,
      test("serveConnection writes 431 when headers exceed maxHeaderBytes"):
        val config = Server.Config.default.copy(maxHeaderBytes = 64)
        runWire(Routes.empty, get("/x", headers = s"X-Big: ${"a" * 200}"), config).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 431"))
        }
      ,
      test("serveConnection writes 400 when headers are truncated"):
        runWire(Routes.empty, "GET /x HTTP/1.1\r\nHost: localhost\r\n").map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 400"))
        }
      ,
      test("collecting a truncated body fails the stream"):
        val routes = Routes(
          Method.POST / "echo" -> handler { (req: Request) =>
            req.body.collect.fold(
              _ => Response.text("bad", Status.BadRequest),
              c => Response.text(String(c.toArray, StandardCharsets.US_ASCII)),
            )
          }
        )
        val raw = "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nab"
        runWire(routes, raw).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 400"))
        }
      ,
      test("SSE is chunked one event per HTTP chunk"):
        val routes = Routes(
          Method.GET / "sse" -> handler(
            ZIO.succeed(
              Sse.response(ZStream(ServerSentEvent("a", event = Some("tick")), ServerSentEvent("b")))
            )
          )
        )
        runWire(routes, get("/sse")).map { wire =>
          val a = String(SseCodec.encode(ServerSentEvent("a", event = Some("tick"))).toArray, StandardCharsets.UTF_8)
          val b = String(SseCodec.encode(ServerSentEvent("b")).toArray, StandardCharsets.UTF_8)
          assertTrue(
            wire.contains("Content-Type: text/event-stream"),
            wire.contains("Cache-Control: no-cache"),
            wire.contains("Transfer-Encoding: chunked"),
            wire.contains(s"${Integer.toHexString(a.length)}\r\n$a\r\n"),
            wire.contains(s"${Integer.toHexString(b.length)}\r\n$b\r\n"),
            wire.endsWith("0\r\n\r\n"),
          )
        }
      ,
      test("unknown-length stream is sent chunked"):
        val routes = Routes(
          Method.GET / "s" -> handler {
            ZIO.succeed(
              Response(Status.Ok).withBody(Body.stream(ZStream.fromChunk(Chunk.fromArray("hello".getBytes))))
            )
          }
        )
        runWire(routes, get("/s")).map { wire =>
          assertTrue(
            wire.contains("Transfer-Encoding: chunked"),
            wire.contains("5\r\nhello\r\n"),
            wire.endsWith("0\r\n\r\n"),
          )
        }
      ,
      test("handler defects become 500"):
        val routes = Routes(Method.GET / "boom" -> Handler.fromZIO(ZIO.dieMessage("nope")))
        runWire(routes, get("/boom")).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 500"))
        }
      ,
      test("serveConnection provides R to the handler"):
        val routes = Routes(
          Method.GET / "n" -> handler { (_: Request) =>
            ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1)).map(n => Response.text(n.toString))
          }
        )
        runWire(routes, get("/n")).provide(ZLayer.fromZIO(Ref.make(0))).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 200"), wire.endsWith("1"))
        }
      ,
      test("read I/O failures are HttpError.Io"):
        val boom   = ZIO.fail(HttpError.Io(IOException("boom")))
        val taking = java.util.concurrent.atomic.AtomicBoolean(true)
        val busy   = java.util.concurrent.atomic.AtomicBoolean(false)
        for err <- Http1.serveConnection(Routes.empty, boom, _ => ZIO.unit, Server.Config.default, taking, busy).flip
        yield err match
          case HttpError.Io(cause) => assertTrue(cause.getMessage == "boom")
          case other               => assertTrue(other.isInstanceOf[HttpError.Io])
      ,
      test("headers split across two reads still parse"):
        val routes = Routes(Method.GET / "a" -> Handler.text("A"))
        val raw    = get("/a")
        val mid    = raw.indexOf("Host:")
        val taking = java.util.concurrent.atomic.AtomicBoolean(true)
        val busy   = java.util.concurrent.atomic.AtomicBoolean(false)
        for
          remaining <- Ref.make(
            Chunk(
              Chunk.fromArray(raw.take(mid).getBytes(StandardCharsets.US_ASCII)),
              Chunk.fromArray(raw.drop(mid).getBytes(StandardCharsets.US_ASCII)),
            )
          )
          out <- Ref.make(Chunk.empty[Byte])
          pull = remaining.modify { c =>
            if c.isEmpty then (None, c) else (Some(c.head), c.drop(1))
          }
          send = (c: Chunk[Byte]) => out.update(_ ++ c).unit
          _     <- Http1.serveConnection(routes, pull, send, Server.Config.default, taking, busy)
          bytes <- out.get
          wire = String(bytes.toArray, StandardCharsets.US_ASCII)
        yield assertTrue(wire.startsWith("HTTP/1.1 200"), wire.endsWith("A"))
        end for
      ,
      test("Host header is available without being interned"):
        val routes = Routes(
          Method.GET / "h" -> handler { (req: Request) =>
            ZIO.succeed(Response.text(req.header("Host").getOrElse("missing")))
          }
        )
        runWire(routes, get("/h", "Host: example.test")).map { wire =>
          assertTrue(wire.endsWith("example.test"))
        }
      ,
      test("keep-alive serves a second request on the same connection"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        runWire(routes, get("/a") + get("/b")).map { wire =>
          assertTrue(
            wire.contains("A"),
            wire.contains("B"),
            wire.indexOf("A") < wire.indexOf("B"),
            !wire.contains("Connection: close"),
          )
        }
      ,
      test("Connection: close does not serve a pipelined second request"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        runWire(routes, get("/a", "Host: localhost\r\nConnection: close") + get("/b")).map { wire =>
          assertTrue(wire.contains("A"), wire.contains("Connection: close"), !wire.contains("B"))
        }
      ,
      test("HTTP/1.0 closes unless Connection: keep-alive"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        runWire(routes, get("/a", "Host: localhost", "HTTP/1.0") + get("/b", "Host: localhost", "HTTP/1.0")).map {
          wire =>
            assertTrue(wire.contains("A"), wire.contains("Connection: close"), !wire.contains("B"))
        }
      ,
      test("HTTP/1.0 keep-alive serves a second request"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        val ka = "Host: localhost\r\nConnection: keep-alive"
        runWire(routes, get("/a", ka, "HTTP/1.0") + get("/b", ka, "HTTP/1.0")).map { wire =>
          assertTrue(
            wire.contains("A"),
            wire.contains("B"),
            wire.contains("Connection: keep-alive"),
            !wire.contains("Connection: close"),
          )
        }
      ,
      test("unread request body is drained before the next request"):
        val routes = Routes(
          Method.POST / "drop" -> Handler.text("dropped"),
          Method.GET / "a"     -> Handler.text("A"),
        )
        val raw =
          "POST /drop HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\n\r\nping" + get("/a")
        runWire(routes, raw).map { wire =>
          assertTrue(wire.contains("dropped"), wire.contains("A"))
        }
      ,
      test("chunked request body is framed so the next request is readable"):
        val routes = Routes(
          Method.POST / "echo" -> handler { (req: Request) =>
            req.body.collect.fold(
              _ => Response.text("bad", Status.BadRequest),
              c => Response.text(String(c.toArray, StandardCharsets.US_ASCII)),
            )
          },
          Method.GET / "a" -> Handler.text("A"),
        )
        val raw =
          "POST /echo HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nping\r\n0\r\n\r\n" +
            get("/a")
        runWire(routes, raw).map { wire =>
          assertTrue(wire.contains("ping"), wire.contains("A"))
        }
      ,
      test("chunked response then a second GET on the same connection"):
        val routes = Routes(
          Method.GET / "s" -> handler {
            ZIO.succeed(Response(Status.Ok).withBody(Body.stream(ZStream.fromChunk(Chunk.fromArray("hello".getBytes)))))
          },
          Method.GET / "a" -> Handler.text("A"),
        )
        runWire(routes, get("/s") + get("/a")).map { wire =>
          assertTrue(wire.contains("Transfer-Encoding: chunked"), wire.contains("hello"), wire.contains("A"))
        }
      ,
      test("protocol error closes and does not parse a pipelined second request"):
        val routes = Routes(Method.GET / "a" -> Handler.text("A"))
        runWire(routes, "GET\r\n\r\n" + get("/a")).map { wire =>
          assertTrue(wire.startsWith("HTTP/1.1 400"), wire.contains("Connection: close"), !wire.contains("A"))
        }
      ,
      test("short Content-Length stream does not keep the connection"):
        val routes = Routes(
          Method.GET / "short" -> handler {
            ZIO.succeed(
              Response(Status.Ok).withBody(
                Body.stream(ZStream.fromChunk(Chunk.fromArray("ab".getBytes)), length = Some(5))
              )
            )
          },
          Method.GET / "a" -> Handler.text("AAA"),
        )
        runWire(routes, get("/short") + get("/a")).either.map {
          case Left(_)     => assertTrue(true)
          case Right(wire) => assertTrue(!wire.contains("AAA"))
        },
    ) @@ TestAspect.timeout(5.seconds)

  private def runWire[R](
      routes: Routes[R, Response],
      raw: String,
      config: Server.Config = Server.Config.default,
  ): ZIO[R, HttpError, String] =
    val taking = java.util.concurrent.atomic.AtomicBoolean(true)
    val busy   = java.util.concurrent.atomic.AtomicBoolean(false)
    for
      remaining <- Ref.make(Chunk.fromArray(raw.getBytes(StandardCharsets.US_ASCII)))
      out       <- Ref.make(Chunk.empty[Byte])
      pull = remaining.modify { c =>
        if c.isEmpty then (None, c) else (Some(c), Chunk.empty)
      }
      send = (c: Chunk[Byte]) => out.update(_ ++ c).unit
      _     <- Http1.serveConnection(routes, pull, send, config, taking, busy)
      bytes <- out.get
    yield String(bytes.toArray, StandardCharsets.US_ASCII)
    end for
  end runWire

  private def get(path: String, headers: String = "Host: localhost", version: String = "HTTP/1.1"): String =
    s"GET $path $version\r\n$headers\r\n\r\n"
end Http1Spec
