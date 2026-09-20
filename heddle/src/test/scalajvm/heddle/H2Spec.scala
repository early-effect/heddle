package heddle

import BytesLength.*
import java.net.Socket
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
      test("SETTINGS round-trips"):
        val f = H2Frame.Settings(ack = false, Chunk(4 -> 65535, 5 -> 16384))
        val e = FrameCodec.encode(f)
        FrameCodec.decode(e, 16384).map { (g, rest) =>
          assertTrue(rest.isEmpty, g == f)
        } match
          case Left(err) => assertTrue(err == "")
          case Right(a)  => a
      ,
      test("HPACK round-trips :method and :path"):
        val hs  = Chunk(":method" -> "GET", ":path" -> "/health", ":scheme" -> "http")
        val dec = Hpack.decode(Hpack.encode(hs))
        assertTrue(dec.contains(":method" -> "GET"), dec.contains(":path" -> "/health"))
      ,
      test("ConnBuf fillUntil sees the h2 preface"):
        val preface = Chunk.fromArray(H2Frame.Preface)
        val rest    = FrameCodec.encode(H2Frame.Settings(ack = false, Chunk.empty))
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
          Client.get(s"$base/health").map(res => assertTrue(res.body.asString == "ok"))
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
              datas.last.isEmpty,
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
      test("H2Flow takeRecv rejects past the stream window"):
        H2Flow.make(16).flatMap { f =>
          f.open(1) *> f.takeRecv(1, 17).map(ok => assertTrue(!ok))
        }
      ,
      test("H2Flow takeSend waits for WINDOW_UPDATE credit"):
        for
          f      <- H2Flow.make(8)
          _      <- f.open(1)
          _      <- f.takeSend(1, 8)
          waiter <- f.takeSend(1, 1).fork
          early  <- waiter.poll
          _      <- f.creditSend(1, 1)
          _      <- waiter.join
        yield assertTrue(early.isEmpty)
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

  private def h2Exchange(
      port: Int,
      reqs: List[H2Req],
      stopAfterData: Int = 0,
      untilRst: Boolean = false,
  ): Task[H2Got] =
    ZIO.attemptBlocking {
      val s = Socket("127.0.0.1", port)
      try
        val out = s.getOutputStream
        val in  = s.getInputStream
        out.write(H2Frame.Preface)
        out.write(FrameCodec.encode(H2Frame.Settings(ack = false, Chunk.empty)).toArray)
        out.flush()
        reqs.foreach { req =>
          val hs =
            Chunk(":method" -> req.method, ":path" -> req.path, ":scheme" -> "http", ":authority" -> "localhost")
          val block = Hpack.encode(hs)
          out.write(
            FrameCodec.encode(H2Frame.Headers(req.id, block, endStream = req.endStream, endHeaders = true)).toArray
          )
          req.body.foreach { b =>
            val bytes = Chunk.fromArray(b.getBytes(StandardCharsets.US_ASCII))
            out.write(FrameCodec.encode(H2Frame.Data(req.id, bytes, endStream = true)).toArray)
          }
        }
        out.flush()
        var buf      = Array.empty[Byte]
        val parts    = scala.collection.mutable.Map.empty[Int, List[String]]
        val rst      = scala.collection.mutable.Map.empty[Int, Int]
        val done     = scala.collection.mutable.Set.empty[Int]
        val want     = reqs.map(_.id).toSet
        var dataSeen = 0
        s.setSoTimeout(3000)
        while
          val waiting = if untilRst then rst.isEmpty else done != want
          waiting && (stopAfterData == 0 || dataSeen < stopAfterData)
        do
          val tmp = Array.ofDim[Byte](4096)
          val n   = in.read(tmp)
          if n < 0 then throw java.io.IOException(s"eof after ${buf.length} got=$parts rst=$rst")
          buf = buf ++ tmp.take(n)
          if buf.length >= 8 && buf(0) == 'H' && buf(1) == 'T' then
            throw java.io.IOException(s"got HTTP/1.1: ${String(buf, StandardCharsets.US_ASCII)}")
          var chunk = Chunk.fromArray(buf)
          var keep  = true
          while keep do
            FrameCodec.decode(chunk, 1 << 20) match
              case Right((H2Frame.Settings(false, _), rest)) =>
                out.write(FrameCodec.encode(H2Frame.Settings(ack = true, Chunk.empty)).toArray)
                out.flush()
                chunk = rest
              case Right((H2Frame.Data(id, data, end, _), rest)) =>
                val sdata = String(data.toArray, StandardCharsets.US_ASCII)
                parts.update(id, parts.getOrElse(id, Nil) :+ sdata)
                if data.nonEmpty then dataSeen += 1
                if end then done += id
                chunk = rest
              case Right((H2Frame.Headers(id, _, end, _, _, _, _), rest)) =>
                if end then
                  parts.getOrElseUpdate(id, Nil)
                  done += id
                chunk = rest
              case Right((H2Frame.RstStream(id, code), rest)) =>
                rst.update(id, code)
                done += id
                chunk = rest
              case Right((_, rest)) =>
                chunk = rest
              case Left(_) =>
                keep = false
          end while
          buf = chunk.toArray
        end while
        H2Got(parts.toMap, rst.toMap)
      finally s.close()
      end try
    }

  private def h2WsEcho(port: Int, path: String, text: String): Task[String] =
    ZIO.attemptBlocking {
      val s = Socket("127.0.0.1", port)
      try
        val out = s.getOutputStream
        val in  = s.getInputStream
        out.write(H2Frame.Preface)
        out.write(FrameCodec.encode(H2Frame.Settings(ack = false, Chunk.empty)).toArray)
        val block = Hpack.encode(
          Chunk(
            ":method"               -> "CONNECT",
            ":protocol"             -> "websocket",
            ":scheme"               -> "http",
            ":path"                 -> path,
            ":authority"            -> "localhost",
            "sec-websocket-version" -> "13",
          )
        )
        out.write(FrameCodec.encode(H2Frame.Headers(1, block, endStream = false, endHeaders = true)).toArray)
        out.flush()
        s.setSoTimeout(3000)
        var buf    = Array.empty[Byte]
        var opened = false
        var echoed = ""
        var done   = false
        while !done do
          val tmp = Array.ofDim[Byte](4096)
          val n   = in.read(tmp)
          if n < 0 then throw java.io.IOException("eof waiting for websocket")
          buf = buf ++ tmp.take(n)
          var chunk = Chunk.fromArray(buf)
          var keep  = true
          while keep do
            FrameCodec.decode(chunk, 1 << 20) match
              case Right((H2Frame.Settings(false, _), rest)) =>
                out.write(FrameCodec.encode(H2Frame.Settings(ack = true, Chunk.empty)).toArray)
                out.flush()
                chunk = rest
              case Right((H2Frame.Headers(_, _, _, _, _, _, _), rest)) =>
                if !opened then
                  opened = true
                  val mask   = Array[Byte](1, 2, 3, 4)
                  val framed =
                    WsCodec.frameBytes(1, text.getBytes(StandardCharsets.UTF_8), fin = true, mask = Some(mask))
                  out.write(FrameCodec.encode(H2Frame.Data(1, framed, endStream = false)).toArray)
                  out.flush()
                chunk = rest
              case Right((H2Frame.Data(_, data, end, _), rest)) =>
                if data.nonEmpty then echoed = echoed + String(data.toArray, StandardCharsets.US_ASCII)
                if end || echoed.contains(text) then done = true
                chunk = rest
              case Right((_, rest)) =>
                chunk = rest
              case Left(_) =>
                keep = false
          end while
          buf = chunk.toArray
        end while
        echoed
      finally s.close()
      end try
    }
end H2Spec
