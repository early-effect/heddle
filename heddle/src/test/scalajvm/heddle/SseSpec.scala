package heddle

import java.nio.charset.StandardCharsets
import heddle.sse.*
import zio.*
import zio.stream.ZStream
import zio.test.*

object SseSpec extends ZIOSpecDefault:
  def spec =
    suite("Sse")(
      test("encode writes event, multiline data, id, retry, and a trailing blank line"):
        val bytes = SseCodec.encode(
          ServerSentEvent(
            data = "hello\nworld",
            event = Some("tick"),
            id = Some("7"),
            retry = Some(2.seconds),
          )
        )
        val text = String(bytes.toArray, StandardCharsets.UTF_8)
        assertTrue(text == "event: tick\ndata: hello\ndata: world\nid: 7\nretry: 2000\n\n")
      ,
      test("encode heartbeat is a comment"):
        val text = String(SseCodec.heartbeat.toArray, StandardCharsets.US_ASCII)
        assertTrue(
          text == ": ping\n\n",
          SseCodec.encode(ServerSentEvent.Heartbeat) == SseCodec.heartbeat,
        )
      ,
      test("decode round-trips a complete event and keeps leftover"):
        val first       = SseCodec.encode(ServerSentEvent("a", event = Some("tick"), id = Some("1")))
        val second      = SseCodec.encode(ServerSentEvent("b"))
        val (evs, rest) = SseCodec.decode(first ++ second.take(4))
        assertTrue(
          evs == Chunk(ServerSentEvent("a", event = Some("tick"), id = Some("1"))),
          rest == second.take(4),
        )
      ,
      test("decode ignores heartbeat comments"):
        val (evs, rest) = SseCodec.decode(SseCodec.heartbeat)
        assertTrue(evs.isEmpty, rest.isEmpty)
      ,
      test("decode concatenates two encoded events"):
        val bytes =
          SseCodec.encode(ServerSentEvent("one")) ++ SseCodec.encode(ServerSentEvent("two"))
        val (evs, rest) = SseCodec.decode(bytes)
        assertTrue(evs == Chunk(ServerSentEvent("one"), ServerSentEvent("two")), rest.isEmpty)
      ,
      test("construction rejects CR or LF in event and id"):
        val event = try
          ServerSentEvent("x", event = Some("a\nb"))
          false
        catch case _: IllegalArgumentException => true
        val id = try
          ServerSentEvent("x", id = Some("a\r"))
          false
        catch case _: IllegalArgumentException => true
        assertTrue(event, id)
      ,
      test("Endpoint.outSse documents text/event-stream"):
        val ep     = Endpoint.get("ticks").outSse
        val routes = ep.implement(_ => ZIO.succeed(ZStream(ServerSentEvent("n", event = Some("tick")))))
        routes(Request.get("/ticks")).map { res =>
          assertTrue(
            res.status == Status.Ok,
            res.header("Content-Type").contains("text/event-stream"),
            ep.doc.responses.exists(_.contentType.contains(MediaType.EventStream)),
          )
        }
      ,
      test("spaced producer advances Clock.nanoTime"):
        for
          q <- Queue.unbounded[Long]
          _ <- ZStream.repeatZIO(Clock.nanoTime).schedule(Schedule.spaced(40.millis)).take(3).foreach(q.offer).fork
          a <- TestClock.adjust(40.millis) *> q.take
          b <- TestClock.adjust(40.millis) *> q.take
          c <- TestClock.adjust(40.millis) *> q.take
        yield assertTrue(b > a, c > b)
      ,
      suite("live")(
        test("Client.sse reads events from LiveServer"):
          val routes = Routes(
            Method.GET / "ticks" -> handler(
              ZIO.succeed(
                Sse.response(
                  ZStream(
                    ServerSentEvent("0", event = Some("tick")),
                    ServerSentEvent("1", event = Some("tick")),
                    ServerSentEvent("2", event = Some("tick")),
                  )
                )
              )
            )
          )
          LiveServer(routes) { base =>
            Client.get(s"$base/ticks").map { res =>
              val (evs, _) = SseCodec.decode(res.body.asBytes)
              assertTrue(
                res.header("Content-Type").exists(_.startsWith("text/event-stream")),
                evs.map(_.data) == Chunk("0", "1", "2"),
              )
            }
          }
        ,
        test("Sse.session writes then ends"):
          val routes = Routes(
            Method.GET / "s" -> handler { (_: Request) =>
              Sse.session { w =>
                w.send(ServerSentEvent("one")) *> w.send(ServerSentEvent("two"))
              }
            }
          )
          LiveServer(routes) { base =>
            Client.get(s"$base/s").map { res =>
              val (evs, _) = SseCodec.decode(res.body.asBytes)
              assertTrue(evs == Chunk(ServerSentEvent("one"), ServerSentEvent("two")))
            }
          }
        ,
        test("client close interrupts a live SSE handler"):
          val routes = Routes(
            Method.GET / "live" -> handler(
              ZIO.succeed(Sse.response(ZStream.tick(20.millis).as(ServerSentEvent("x"))))
            )
          )
          LiveServer(routes) { base =>
            Client.sse(s"$base/live").take(1).runCollect.map { evs =>
              assertTrue(evs == Chunk(ServerSentEvent("x")))
            }
          }
        ,
        test("shutdown does not hang on an open SSE stream"):
          val routes = Routes(
            Method.GET / "live" -> handler(
              ZIO.succeed(Sse.response(ZStream.tick(50.millis).as(ServerSentEvent("x"))))
            )
          )
          ZIO
            .scoped {
              Server.install(routes, LiveServer.local).flatMap { server =>
                server.port.flatMap { port =>
                  Client.sse(s"http://127.0.0.1:$port/live").take(1).runCollect
                }
              }
            }
            .map(evs => assertTrue(evs == Chunk(ServerSentEvent("x")))),
      ) @@ TestAspect.sequential @@ TestAspect.timeout(5.seconds) @@ TestAspect.withLiveClock,
    ) @@ TestAspect.timeout(5.seconds)
end SseSpec
