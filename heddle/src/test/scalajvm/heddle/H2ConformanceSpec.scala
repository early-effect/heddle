package heddle

import heddle.error.WireError
import heddle.internal.h2.{FrameCodec, H2Frame}
import java.net.Socket
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

  private final class Wire(socket: Socket, buffer: Ref[Chunk[Byte]], seen: Ref[Chunk[H2Frame]]):
    def send(frames: H2Frame*): Task[Unit] =
      ZIO.attemptBlocking {
        socket.getOutputStream.write(Chunk.fromIterable(frames).flatMap(FrameCodec.encode).toArray)
        socket.getOutputStream.flush()
      }

    /** Every frame the server has sent once `done` holds for them, acknowledging its SETTINGS on the way. */
    def until(done: Chunk[H2Frame] => Boolean): Task[Chunk[H2Frame]] =
      seen.get.flatMap { frames =>
        if done(frames) then ZIO.succeed(frames)
        else
          buffer.get.flatMap { buf =>
            FrameCodec.decode(buf, 1 << 24) match
              case Right((frame, rest)) =>
                val ack = frame match
                  case H2Frame.Settings(_) => send(H2Frame.SettingsAck)
                  case _                   => ZIO.unit
                buffer.set(rest) *> seen.update(_ :+ frame) *> ack *> until(done)
              case Left(WireError.TruncatedFrame) =>
                ZIO
                  .attemptBlocking {
                    val tmp = new Array[Byte](65536)
                    val n   = socket.getInputStream.read(tmp)
                    Chunk.fromArray(tmp.take(n.max(0))) -> (n < 0)
                  }
                  .flatMap { (more, eof) =>
                    if eof then ZIO.succeed(frames) else buffer.update(_ ++ more) *> until(done)
                  }
              case Left(e) => ZIO.fail(java.io.IOException(e.message))
          }
      }
  end Wire

  private def wire[A](port: Int, settings: Chunk[(Int, Int)] = Chunk.empty)(f: Wire => Task[A]): Task[A] =
    ZIO.scoped {
      for
        socket <- ZIO.fromAutoCloseable(ZIO.attemptBlocking(Socket("127.0.0.1", port)))
        _      <- ZIO.attemptBlocking(socket.setSoTimeout(5000))
        w      <- (Ref.make(Chunk.empty[Byte]) <*> Ref.make(Chunk.empty[H2Frame])).map(Wire(socket, _, _))
        _      <- ZIO.attemptBlocking(socket.getOutputStream.write(H2Frame.Preface))
        _      <- w.send(H2Frame.Settings(settings))
        a      <- f(w)
      yield a
    }

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
        wire(java.net.URI.create(base).getPort) { w =>
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
        wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, first, endStream = true, endHeaders = true)) *> w.until(ended(1)) *>
            w.send(H2Frame.Headers(3, second, endStream = true, endHeaders = true)) *>
            w.until(ended(3)).map(frames => assertTrue(body(frames, 1) == "abc", body(frames, 3) == "abc"))
        }
      }
    ,
    test("a frame between HEADERS and its CONTINUATION is a PROTOCOL_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, get("/t").take(3), endStream = true, endHeaders = false), H2Frame.Ping(7L)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x1)))
        }
      }
    ,
    test("an index outside both HPACK tables is a COMPRESSION_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(1, get("/t") ++ Chunk(0xbe.toByte), endStream = true, endHeaders = true)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x9)))
        }
      }
    ,
    test("responses follow the peer's SETTINGS: its frame size and its initial window"):
      LiveServer(echo, LiveServer.local) { base =>
        wire(java.net.URI.create(base).getPort, settings = Chunk(4 -> 100000, 5 -> 32768)) { w =>
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
          wire(java.net.URI.create(base).getPort) { w =>
            w.send(H2Frame.Headers(1, get("/hold"), endStream = true, endHeaders = true)) *> started.await *>
              w.send(H2Frame.RstStream(1, 0x8)) *> interrupted.await.timeout(5.seconds)
          }
        }
      yield assertTrue(out.isDefined)
    ,
    test("a stream id lower than one already opened is a PROTOCOL_ERROR"):
      LiveServer(echo, LiveServer.local) { base =>
        wire(java.net.URI.create(base).getPort) { w =>
          w.send(H2Frame.Headers(5, get("/t"), endStream = true, endHeaders = true)) *> w.until(ended(5)) *>
            w.send(H2Frame.Headers(3, get("/t"), endStream = true, endHeaders = true)) *>
            w.until(goAway(_).isDefined).map(frames => assertTrue(goAway(frames).contains(0x1)))
        }
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
end H2ConformanceSpec
