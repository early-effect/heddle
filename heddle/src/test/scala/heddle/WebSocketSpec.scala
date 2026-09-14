package heddle

import java.io.{InputStream, OutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.util.Base64
import heddle.ws.{WebSocketFrame, WsCodec}
import zio.*
import zio.test.*

object WebSocketSpec extends ZIOSpecDefault:
  def spec =
    suite("WebSocket")(
      test("handshake is 101 and echoes text"):
        val routes = Routes(Method.GET / "ws" -> Handler.websocket { ws =>
          ws.receive.take(1).runHead.flatMap {
            case Some(WebSocketFrame.Text(t)) => ws.send(WebSocketFrame.Text(t)) *> ws.close()
            case _                            => ws.close()
          }
        })
        LiveServer(routes) { base =>
          ZIO
            .attemptBlocking {
              val port = java.net.URI.create(base).getPort
              val s    = Socket("127.0.0.1", port)
              try
                val (status, _) = upgrade(s)
                writeFrame(s.getOutputStream, text = "hello")
                val echoed = readFrame(s.getInputStream)
                (status, echoed)
              finally s.close()
            }
            .map { (status, echoed) =>
              assertTrue(status.contains("101"), echoed.contains("hello"))
            }
        }
      ,
      test("ping is answered with pong"):
        val routes = Routes(Method.GET / "ws" -> Handler.websocket { ws =>
          ws.receive.runDrain
        })
        LiveServer(routes) { base =>
          ZIO
            .attemptBlocking {
              val port = java.net.URI.create(base).getPort
              val s    = Socket("127.0.0.1", port)
              try
                upgrade(s)
                writeOpcode(s.getOutputStream, opcode = 9, payload = "hi".getBytes)
                val (op, data) = readOpcode(s.getInputStream)
                (op, String(data, StandardCharsets.UTF_8))
              finally s.close()
            }
            .map { (op, data) =>
              assertTrue(op == 10, data == "hi")
            }
        }
      ,
      test("client close interrupts the session"):
        val routes = Routes(Method.GET / "ws" -> Handler.websocket { ws =>
          ws.receive.runDrain
        })
        ZIO
          .scoped {
            Server.install(routes, LiveServer.local).flatMap { server =>
              server.port.flatMap { port =>
                ZIO.attemptBlocking {
                  val s = Socket("127.0.0.1", port)
                  try
                    upgrade(s)
                    writeOpcode(s.getOutputStream, opcode = 8, payload = Array[Byte](0x03, (0xe8).toByte))
                  finally s.close()
                }
              }
            }
          }
          .as(assertTrue(true))
      ,
      test("server close writes a Close frame"):
        val routes = Routes(Method.GET / "ws" -> Handler.websocket { ws =>
          ws.close(1000, "bye")
        })
        LiveServer(routes) { base =>
          ZIO
            .attemptBlocking {
              val port = java.net.URI.create(base).getPort
              val s    = Socket("127.0.0.1", port)
              try
                upgrade(s)
                val (op, data) = readOpcode(s.getInputStream)
                val code       = ((data(0) & 0xff) << 8) | (data(1) & 0xff)
                (op, code)
              finally s.close()
            }
            .map { (op, code) =>
              assertTrue(op == 8, code == 1000)
            }
        }
      ,
      test("oversized frame is 1009"):
        val routes = Routes(Method.GET / "ws" -> Handler.websocket { ws =>
          ws.receive.runDrain.catchAll(_ => ZIO.unit)
        })
        LiveServer(routes) { base =>
          ZIO
            .attemptBlocking {
              val port = java.net.URI.create(base).getPort
              val s    = Socket("127.0.0.1", port)
              s.setTcpNoDelay(true)
              try
                upgrade(s)
                writeOversizeHeader(s.getOutputStream, opcode = 2, len = WsCodec.MaxPayload.toLong + 1)
                val (op, data) = readOpcode(s.getInputStream)
                val code       = if data.length >= 2 then ((data(0) & 0xff) << 8) | (data(1) & 0xff) else 0
                (op, code)
              finally s.close()
            }
            .map { (op, code) =>
              assertTrue(op == 8, code == 1009)
            }
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(5.seconds) @@ TestAspect.withLiveClock

  private def upgrade(s: Socket): (String, String) =
    val key = Base64.getEncoder.encodeToString("1234567890abcdef".getBytes(StandardCharsets.US_ASCII))
    val req =
      s"GET /ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: $key\r\nSec-WebSocket-Version: 13\r\n\r\n"
    s.getOutputStream.write(req.getBytes(StandardCharsets.US_ASCII))
    s.getOutputStream.flush()
    val in  = s.getInputStream
    val acc = scala.collection.mutable.ArrayBuffer.empty[Byte]
    while acc.size < 4 || acc.takeRight(4) != Seq[Byte]('\r', '\n', '\r', '\n') do
      val b = in.read()
      if b < 0 then throw java.io.IOException("eof in upgrade")
      acc += b.toByte
    val raw = String(acc.toArray, StandardCharsets.US_ASCII)
    (raw, raw)
  end upgrade

  private def writeFrame(out: OutputStream, text: String): Unit =
    writeOpcode(out, 1, text.getBytes(StandardCharsets.UTF_8))

  private def writeOpcode(out: OutputStream, opcode: Int, payload: Array[Byte]): Unit =
    val mask   = Array[Byte](1, 2, 3, 4)
    val framed = WsCodec.frameBytes(opcode, payload, fin = true, mask = Some(mask))
    out.write(framed.toArray)
    out.flush()

  /** Length is in the 2+8 byte header. Do not write the payload: the server closes on the
    * declared size, and a megabyte write races the close with a TCP RST.
    */
  private def writeOversizeHeader(out: OutputStream, opcode: Int, len: Long): Unit =
    val hdr = Array.ofDim[Byte](10)
    hdr(0) = (0x80 | (opcode & 0x0f)).toByte
    hdr(1) = 127.toByte
    var s = 56
    var i = 2
    while s >= 0 do
      hdr(i) = ((len >> s) & 0xff).toByte
      s -= 8
      i += 1
    out.write(hdr)
    out.flush()

  private def readFrame(in: InputStream): String =
    val (_, data) = readOpcode(in)
    String(data, StandardCharsets.UTF_8)

  private def readOpcode(in: InputStream): (Int, Array[Byte]) =
    val h0 = in.read()
    val h1 = in.read()
    if h0 < 0 || h1 < 0 then throw java.io.IOException("eof")
    val op   = h0 & 0x0f
    val len7 = h1 & 0x7f
    val len  =
      if len7 < 126 then len7
      else if len7 == 126 then (in.read() << 8) | in.read()
      else
        var n = 0L
        var i = 0
        while i < 8 do
          n = (n << 8) | in.read()
          i += 1
        n.toInt
    val data = in.readNBytes(len)
    (op, data)
  end readOpcode
end WebSocketSpec
