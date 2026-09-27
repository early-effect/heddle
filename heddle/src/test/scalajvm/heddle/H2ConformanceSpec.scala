package heddle

import heddle.internal.h2.H2Frame
import java.nio.charset.StandardCharsets
import zio.*
import zio.test.*

/** RFC 9113 behaviour a real client relies on, driven with raw frames over a socket. */
object H2ConformanceSpec extends ZIOSpecDefault:
  private def ascii(s: String): Chunk[Byte] = Chunk.fromArray(s.getBytes(StandardCharsets.US_ASCII))

  /** HPACK literal fields with a literal name: `0x00` keeps the table as it is, `0x40` adds the field to it. */
  private def literal(name: String, value: String, index: Boolean = false): Chunk[Byte] =
    Chunk((if index then 0x40 else 0x00).toByte, name.length.toByte) ++ ascii(name) ++
      Chunk(value.length.toByte) ++ ascii(value)

  private def get(path: String): Chunk[Byte] =
    Chunk(0x82.toByte, 0x86.toByte) ++ literal(":path", path) ++ literal(":authority", "localhost")

  private def post(path: String): Chunk[Byte] =
    Chunk(0x83.toByte, 0x86.toByte) ++ literal(":path", path) ++ literal(":authority", "localhost")

  private def body(frames: Chunk[H2Frame], stream: Int): String =
    frames.collect { case H2Frame.Data(`stream`, d, _, _) => String(d.toArray, StandardCharsets.US_ASCII) }.mkString

  private def ended(stream: Int)(frames: Chunk[H2Frame]): Boolean =
    frames.exists {
      case H2Frame.Data(`stream`, _, true, _)             => true
      case H2Frame.Headers(`stream`, _, true, _, _, _, _) => true
      case _                                              => false
    }

  private def goAway(frames: Chunk[H2Frame]): Option[Int] =
    frames.collectFirst { case H2Frame.GoAway(_, code, _) => code }

  private val echo = Routes(
    Method.POST / "echo" -> handler((req: Request) =>
      req.body.collect.fold(_ => Response.text("unreadable", Status.BadRequest), c => Response.text(s"[${c.length}]"))
    ),
    Method.GET / "t" -> handler((req: Request) => ZIO.succeed(Response.text(req.header("x-token").getOrElse("none")))),
    Method.GET / "big" -> Handler.text("x" * 80000),
  )

  def spec = suite("HTTP/2 conformance")(
    test("END_STREAM on a HEADERS that CONTINUATION finishes ends the request body"):
      LiveServer(echo, LiveServer.local) { base =>
        val block = post("/echo")
        H2Wire(java.net.URI.create(base).getPort) { w =>
          w.send(
            H2Frame.Headers(1, block.take(4), endStream = true, endHeaders = false),
            H2Frame.Continuation(1, block.drop(4), endHeaders = true),
          ) *> w.until(ended(1)).map(frames => assertTrue(body(frames, 1) == "[0]"))
        }
      }
    ,
    test("a later request's headers can name entries an earlier one added to the HPACK table"):
      LiveServer(echo, LiveServer.local) { base =>
        val first  = get("/t") ++ literal("x-token", "abc", index = true)
        val second = get("/t") ++ Chunk(0xbe.toByte)
        H2Wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, first, endStream = true, endHeaders = true)) *> w.until(ended(1)) *>
            w.send(H2Frame.Headers(3, second, endStream = true, endHeaders = true)) *>
            w.until(ended(3)).map(frames => assertTrue(body(frames, 1) == "abc", body(frames, 3) == "abc"))
        }
      }
    ,
    test("a frame between HEADERS and its CONTINUATION is a PROTOCOL_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        H2Wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, get("/t").take(3), endStream = true, endHeaders = false), H2Frame.Ping(7L)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x1)))
        }
      }
    ,
    test("an index outside both HPACK tables is a COMPRESSION_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        H2Wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, get("/t") ++ Chunk(0xbe.toByte), endStream = true, endHeaders = true)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x9)))
        }
      }
    ,
    test("responses follow the peer's SETTINGS: its frame size and its initial window"):
      LiveServer(echo, LiveServer.local) { base =>
        H2Wire(java.net.URI.create(base).getPort, settings = Chunk(4 -> 100000, 5 -> 32768)) { w =>
          w.send(
            H2Frame.WindowUpdate(0, 100000),
            H2Frame.Headers(1, get("/big"), endStream = true, endHeaders = true),
          ) *> w.until(ended(1)).map { frames =>
            val sizes = frames.collect { case H2Frame.Data(1, d, _, _) if d.nonEmpty => d.length }
            assertTrue(sizes.sum == 80000, sizes.max > 16384, sizes.max <= 32768)
          }
        }
      }
    ,
    test("a peer's RST_STREAM interrupts that stream's handler"):
      for
        started     <- Promise.make[Nothing, Unit]
        interrupted <- Promise.make[Nothing, Unit]
        routes = Routes(
          Method.GET / "hold" -> handler(started.succeed(()) *> ZIO.never.onInterrupt(interrupted.succeed(())))
        )
        out <- LiveServer(routes, LiveServer.local) { base =>
          H2Wire(java.net.URI.create(base).getPort) { w =>
            w.send(H2Frame.Headers(1, get("/hold"), endStream = true, endHeaders = true)) *> started.await *>
              w.send(H2Frame.RstStream(1, 0x8)) *> interrupted.await.timeout(5.seconds)
          }
        }
      yield assertTrue(out.isDefined)
    ,
    test("a stream id lower than one already opened is a PROTOCOL_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        H2Wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(5, get("/t"), endStream = true, endHeaders = true)) *> w.until(ended(5)) *>
            w.send(H2Frame.Headers(3, get("/t"), endStream = true, endHeaders = true)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x1)))
        }
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
end H2ConformanceSpec
