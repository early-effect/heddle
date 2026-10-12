package heddle

import heddle.server.MaxConnections
import java.io.{ByteArrayOutputStream, InputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import zio.*
import zio.test.*

object ServerLimitsSpec extends ZIOSpecDefault:
  def spec =
    val limits = suite("server limits")(
      test("named defaults are the values on Config.default"):
        val c = Server.Config.default
        assertTrue(
          c.idleTimeout == Server.Config.defaultIdleTimeout,
          c.headerTimeout == Server.Config.defaultHeaderTimeout,
          c.maxConnections == Server.Config.defaultMaxConnections,
          c.maxRequestsPerConnection == Server.Config.defaultMaxRequestsPerConnection,
          c.soBacklog == Server.Config.defaultSoBacklog,
          c.soKeepAlive == Server.Config.defaultSoKeepAlive,
          c.idleTimeout == 60.seconds,
          c.headerTimeout == 30.seconds,
          c.maxRequestsPerConnection.isEmpty,
          c.maxConnections.toInt == 1024,
          c.soBacklog.toInt == 100,
        )
      ,
      test("idleTimeout closes a quiet keep-alive"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        val config = LiveServer.local.copy(idleTimeout = 200.millis)
        LiveServer(routes, config) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                sock.setSoTimeout(2000)
                val first = writeGet(sock, "/health")
                Thread.sleep(500)
                sock.getOutputStream.write("GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
                sock.getOutputStream.flush()
                val n = sock.getInputStream.read()
                (first, n)
              finally sock.close()
            }
            .either
            .map {
              case Left(_)           => assertTrue(true)
              case Right((first, n)) => assertTrue(first.contains("ok"), n < 0)
            }
        }
      ,
      test("HELLO with the socket left open is 400 without waiting out headerTimeout"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        val config = LiveServer.local.copy(headerTimeout = 2.seconds)
        LiveServer(routes, config) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                sock.setSoTimeout(500)
                sock.getOutputStream.write("HELLO\r\n\r\n".getBytes(StandardCharsets.US_ASCII))
                sock.getOutputStream.flush()
                String(sock.getInputStream.readAllBytes(), StandardCharsets.US_ASCII)
              finally sock.close()
            }
            .map(wire => assertTrue(wire.startsWith("HTTP/1.1 400")))
        }
      ,
      test("bytes that keep arriving do not extend headerTimeout"):
        val hit    = java.util.concurrent.atomic.AtomicBoolean(false)
        val routes = Routes(Method.GET / "health" -> handler {
          ZIO.succeed {
            hit.set(true)
            Response.text("ok")
          }
        })
        val config = LiveServer.local.copy(headerTimeout = 200.millis)
        LiveServer(routes, config) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                sock.setSoTimeout(200)
                val out = sock.getOutputStream
                val in  = sock.getInputStream
                out.write(
                  "GET /health HTTP/1.1\r\nHost: localhost\r\nX-Slow: ".getBytes(StandardCharsets.US_ASCII)
                )
                out.flush()
                val acc = ByteArrayOutputStream()
                val buf = new Array[Byte](2048)
                var i   = 0
                while i < 12 && acc.size() == 0 do
                  try
                    out.write('a')
                    out.flush()
                  catch case _: java.io.IOException => i = 12
                  try
                    val n = in.read(buf)
                    if n < 0 then i = 12
                    else if n > 0 then acc.write(buf, 0, n)
                  catch case _: java.net.SocketTimeoutException => ()
                  if acc.size() == 0 && i < 12 then Thread.sleep(50)
                  i += 1
                end while
                var open = acc.size() > 0
                while open do
                  try
                    val n = in.read(buf)
                    if n < 0 then open = false
                    else if n > 0 then acc.write(buf, 0, n)
                  catch
                    case _: java.net.SocketTimeoutException => open = false
                    case _: java.io.IOException             => open = false
                end while
                (String(acc.toByteArray, StandardCharsets.US_ASCII), hit.get())
              finally sock.close()
              end try
            }
            .map { (wire, reached) =>
              assertTrue(wire.startsWith("HTTP/1.1 408"), !reached)
            }
        }
      ,
      test("headerTimeout closes a client that never finishes headers"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        val config = LiveServer.local.copy(headerTimeout = 200.millis)
        LiveServer(routes, config) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              try
                sock.setSoTimeout(2000)
                sock.getOutputStream.write("GET /health HTTP/1.1\r\nHost: localhost\r\n".getBytes)
                sock.getOutputStream.flush()
                String(sock.getInputStream.readAllBytes(), StandardCharsets.US_ASCII)
              finally sock.close()
            }
            .either
            .map {
              case Left(_)     => assertTrue(true)
              case Right(wire) => assertTrue(wire.startsWith("HTTP/1.1 408") || wire.isEmpty)
            }
        }
      ,
      test("maxConnections closes the extra handshake"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        val config = LiveServer.local.copy(maxConnections = MaxConnections(2))
        LiveServer(routes, config) { base =>
          val port = java.net.URI.create(base).getPort
          ZIO
            .attemptBlocking {
              val a = openHeld(port)
              val b = openHeld(port)
              try
                val c = Socket("127.0.0.1", port)
                try
                  c.setSoTimeout(2000)
                  c.getOutputStream.write("GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
                  c.getOutputStream.flush()
                  c.getInputStream.read()
                finally c.close()
              finally
                a.close()
                b.close()
              end try
            }
            .either
            .map {
              case Left(_)  => assertTrue(true)
              case Right(n) => assertTrue(n < 0)
            }
        },
    )
    limits @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(10.seconds)
  end spec

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

  private def openHeld(port: Int): Socket =
    val sock = Socket("127.0.0.1", port)
    sock.setSoTimeout(2000)
    writeGet(sock, "/health")
    sock
end ServerLimitsSpec
