package heddle

import BytesLength.*
import java.nio.charset.StandardCharsets
import heddle.internal.h2.{FrameCodec, H2Flow, H2Frame, Hpack}
import heddle.internal.engine.ConnBuf
import heddle.ws.{WebSocketFrame, WsCodec}
import zio.*
import zio.test.*

object H2Spec extends ZIOSpecDefault:
  def spec =
    suite("H2")(
      test("preface is 24 bytes and http2 is on by default"):
        assertTrue(H2Frame.Preface.length == 24, Server.Config.default.http2)
      ,
      test("ConnBuf fillUntil sees the h2 preface"):
        val preface = Chunk.fromArray(H2Frame.Preface)
        val rest    = FrameCodec.encode(H2Frame.Settings(Chunk.empty))
        val src     = ConnBuf.fromPull(java.nio.ByteBuffer.allocate(1024), ZIO.succeed(Some(preface ++ rest)))
        src.fillUntil(24).map { avail =>
          val peek = src.peek(24)
          assertTrue(
            avail >= 24,
            src.hasPrefix(H2Frame.Preface),
            peek == preface,
          )
        }
      ,
      test("same port still serves HTTP/1.1"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          Client.get(s"$base/health").map(res => assertTrue(res.body.text.is(_.some) == "ok"))
        }
      ,
      test("prior-knowledge h2c GET hits the same routes"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          h2Get(port, "/health").map { body =>
            assertTrue(body.contains("ok"))
          }
        }
      ,
      test("two concurrent streams both complete"):
        val routes = Routes(
          Method.GET / "a" -> Handler.text("A"),
          Method.GET / "b" -> Handler.text("B"),
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          h2Gets(port, List(1 -> "/a", 3 -> "/b")).map { m =>
            assertTrue(m.get(1).exists(_.contains("A")), m.get(3).exists(_.contains("B")))
          }
        }
      ,
      test("RFC 8441 extended CONNECT websocket echoes text"):
        val routes = Routes(
          Method.GET / "ws" -> Handler.websocket { ws =>
            ws.receive.take(1).runHead.flatMap {
              case Some(WebSocketFrame.Text(t)) => ws.send(WebSocketFrame.Text(t)) *> ws.close()
              case _                            => ws.close()
            }
          }
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          h2WsEcho(port, "/ws", "hello").map(t => assertTrue(t.contains("hello")))
        }
      ,
      test("SSE-on-H2 writes one DATA per event then empty END_STREAM"):
        val routes = Routes(
          Method.GET / "sse" -> handler(
            ZIO.succeed(
              heddle.sse.Sse.response(
                zio.stream.ZStream(
                  heddle.sse.ServerSentEvent("a"),
                  heddle.sse.ServerSentEvent("b"),
                )
              )
            )
          )
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          h2Exchange(port, List(H2Req.get(1, "/sse"))).map { got =>
            val datas = got.datas(1)
            val body  = datas.mkString
            assertTrue(
              datas.count(_.nonEmpty) == 2,
              datas.lastOption.exists(_.isEmpty),
              body.contains("data: a"),
              body.contains("data: b"),
            )
          }
        }
      ,
      test("body over maxBodyBytes is RST_STREAM"):
        val routes = Routes(
          Method.POST / "echo" -> handler((req: Request) => req.body.collect.orDie.as(Response.ok))
        )
        val cfg = LiveServer.local.copy(maxBodyBytes = 8.B)
        LiveServer(routes, cfg) { base =>
          val port = java.net.URI.create(base).getPort
          h2Exchange(port, List(H2Req.post(1, "/echo", "0123456789abcdef")), untilRst = true).map { got =>
            assertTrue(got.rst.get(1).contains(0x7))
          }
        }
      ,
      test("DATA past the receive window is FLOW_CONTROL_ERROR"):
        val routes = Routes(
          Method.POST / "echo" -> handler((req: Request) => req.body.collect.orDie.as(Response.ok))
        )
        val cfg = LiveServer.local.copy(http2Config = Http2Config(initialWindowSize = 16.B))
        LiveServer(routes, cfg) { base =>
          val port = java.net.URI.create(base).getPort
          h2Exchange(port, List(H2Req.post(1, "/echo", "0123456789abcdef0123456789abcdef")), untilRst = true).map {
            got =>
              assertTrue(got.rst.get(1).contains(H2Flow.FlowControlError))
          }
        }
      ,
      test("maxConcurrentStreams refuses the extra stream"):
        val routes = Routes(
          Method.GET / "hold" -> handler(ZIO.never),
          Method.GET / "b"    -> Handler.text("B"),
        )
        val cfg = LiveServer.local.copy(http2Config = Http2Config(maxConcurrentStreams = 1))
        LiveServer(routes, cfg) { base =>
          val port = java.net.URI.create(base).getPort
          h2Exchange(port, List(H2Req.get(1, "/hold"), H2Req.get(3, "/b")), untilRst = true).map { got =>
            assertTrue(got.rst.values.exists(_ == H2Flow.RefusedStream))
          }
        }
      ,
      test("shutdown does not hang on an open H2 SSE stream"):
        val routes = Routes(
          Method.GET / "live" -> handler(
            ZIO.succeed(
              heddle.sse.Sse.response(
                zio.stream.ZStream.tick(50.millis).as(heddle.sse.ServerSentEvent("x"))
              )
            )
          )
        )
        ZIO
          .scoped {
            Server.install(routes, LiveServer.local).flatMap { server =>
              server.port.flatMap { port =>
                h2Exchange(port, List(H2Req.get(1, "/live")), stopAfterData = 1)
              }
            }
          }
          .map(got => assertTrue(got.datas(1).exists(_.contains("data: x")))),
    ) @@ TestAspect.sequential @@ TestAspect.timeout(8.seconds) @@ TestAspect.withLiveClock

  private def h2Get(port: Int, path: String): Task[String] =
    h2Gets(port, List(1 -> path)).map(_.getOrElse(1, ""))

  private def h2Gets(port: Int, streams: List[(Int, String)]): Task[Map[Int, String]] =
    h2Exchange(port, streams.map((id, path) => H2Req.get(id, path))).map { got =>
      got.body.map((id, parts) => id -> parts.mkString).toMap
    }

  private final case class H2Req(id: Int, method: String, path: String, body: Option[String]):
    def endStream: Boolean = body.isEmpty

  private object H2Req:
    def get(id: Int, path: String): H2Req                = H2Req(id, "GET", path, None)
    def post(id: Int, path: String, body: String): H2Req = H2Req(id, "POST", path, Some(body))

  private final case class H2Got(
      body: Map[Int, List[String]],
      rst: Map[Int, Int],
  ):
    def datas(id: Int): List[String] = body.getOrElse(id, Nil)

  /** Sends each request, then reads until every stream ends (or one is reset, or `stopAfterData` DATA frames came). */
  private def h2Exchange(
      port: Int,
      reqs: List[H2Req],
      stopAfterData: Int = 0,
      untilRst: Boolean = false,
  ): Task[H2Got] =
    val want   = reqs.map(_.id).toSet
    val frames = reqs.flatMap { req =>
      val block = Hpack.encode(
        Chunk(":method" -> req.method, ":path" -> req.path, ":scheme" -> "http", ":authority" -> "localhost")
      )
      H2Frame.Headers(req.id, block, endStream = req.endStream, endHeaders = true) ::
        req.body
          .map(b => H2Frame.Data(req.id, Chunk.fromArray(b.getBytes(StandardCharsets.US_ASCII)), endStream = true))
          .toList
    }
    def reset(fs: Chunk[H2Frame]) = fs.collect { case H2Frame.RstStream(id, code) => id -> code }.toMap
    def ended(fs: Chunk[H2Frame]) =
      fs.collect {
        case H2Frame.Data(id, _, true, _)             => id
        case H2Frame.Headers(id, _, true, _, _, _, _) => id
        case H2Frame.RstStream(id, _)                 => id
      }.toSet
    def dataSeen(fs: Chunk[H2Frame]) = fs.count { case H2Frame.Data(_, d, _, _) => d.nonEmpty; case _ => false }
    H2Wire(port) { w =>
      w.send(frames*) *> w
        .until { fs =>
          (if untilRst then reset(fs).nonEmpty else ended(fs) == want) ||
          (stopAfterData > 0 && dataSeen(fs) >= stopAfterData)
        }
        .map { fs =>
          val parts = fs.collect { case H2Frame.Data(id, d, _, _) =>
            id -> String(d.toArray, StandardCharsets.US_ASCII)
          }
          H2Got(parts.groupMap(_._1)(_._2).map((id, ds) => id -> ds.toList), reset(fs))
        }
    }
  end h2Exchange

  /** Opens an RFC 8441 WebSocket over stream 1, sends `text` once the server answers, and reads its echo. */
  private def h2WsEcho(port: Int, path: String, text: String): Task[String] =
    val open = Hpack.encode(
      Chunk(
        ":method"               -> "CONNECT",
        ":protocol"             -> "websocket",
        ":scheme"               -> "http",
        ":path"                 -> path,
        ":authority"            -> "localhost",
        "sec-websocket-version" -> "13",
      )
    )
    val framed =
      WsCodec.frameBytes(1, text.getBytes(StandardCharsets.UTF_8), fin = true, mask = Some(Array[Byte](1, 2, 3, 4)))
    def echoed(fs: Chunk[H2Frame]) =
      fs.collect { case H2Frame.Data(1, d, _, _) => String(d.toArray, StandardCharsets.US_ASCII) }.mkString
    H2Wire(port) { w =>
      w.send(H2Frame.Headers(1, open, endStream = false, endHeaders = true)) *>
        w.until(_.exists { case H2Frame.Headers(1, _, _, _, _, _, _) => true; case _ => false }) *>
        w.send(H2Frame.Data(1, framed, endStream = false)) *>
        w.until(fs => echoed(fs).contains(text)).map(echoed)
    }
  end h2WsEcho
end H2Spec
