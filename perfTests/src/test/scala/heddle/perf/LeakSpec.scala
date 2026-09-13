package heddle.perf

import java.net.Socket
import java.nio.charset.StandardCharsets
import heddle.*
import zio.*
import zio.stream.ZStream
import zio.test.*

object LeakSpec extends ZIOSpecDefault:
  def spec =
    suite("leaks")(
      test("sequential connections return TCP and heap to baseline"):
        cycle(n = 32, keepAlive = false)
      ,
      test("concurrent keep-alive connections return TCP and heap to baseline"):
        soakKeepAlive(n = 32)
      ,
      test("Connection: close does not leak a descriptor per request"):
        cycle(n = 32, keepAlive = false)
      ,
      test("client abort mid-body does not leak the channel"):
        val routes = Routes(
          Method.POST / "echo" -> handler { (req: Request) =>
            req.body.collect.fold(_ => Response.badRequest("bad"), _ => Response.text("ok"))
          },
          Method.GET / "health" -> Handler.text("ok"),
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          for
            _     <- Client.get(s"$base/health")
            _     <- ZIO.succeed(Resources.gc())
            tcp0  <- ZIO.succeed(Resources.establishedTcp)
            heap0 <- ZIO.succeed(Resources.heapUsed)
            _     <- ZIO.attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              sock.getOutputStream.write(
                "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 10000\r\n\r\nab".getBytes(StandardCharsets.US_ASCII)
              )
              sock.getOutputStream.flush()
              sock.close()
            }
            _     <- Client.get(s"$base/health")
            _     <- ZIO.succeed(Resources.gc())
            tcp1  <- ZIO.succeed(Resources.establishedTcp)
            heap1 <- ZIO.succeed(Resources.heapUsed)
          yield leaked(tcp0, tcp1, heap0, heap1)
        }
      ,
      test("idle keep-alive then client close returns TCP to baseline"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          for
            _     <- Client.get(s"$base/health")
            _     <- ZIO.succeed(Resources.gc())
            tcp0  <- ZIO.succeed(Resources.establishedTcp)
            heap0 <- ZIO.succeed(Resources.heapUsed)
            _     <- ZIO.attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              sock.getOutputStream.write("GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
              sock.getOutputStream.flush()
              sock.getInputStream.read()
              sock.close()
            } *> ZIO.sleep(100.millis)
            _     <- ZIO.succeed(Resources.gc())
            tcp1  <- ZIO.succeed(Resources.establishedTcp)
            heap1 <- ZIO.succeed(Resources.heapUsed)
          yield leaked(tcp0, tcp1, heap0, heap1)
        }
      ,
      test("client disconnect on a streaming response does not leak"):
        val routes = Routes(
          Method.GET / "stream" -> handler {
            ZIO.succeed(Response(Status.Ok).withBody(Body.stream(ZStream.repeat(0x78.toByte).take(1_000_000))))
          },
          Method.GET / "health" -> Handler.text("ok"),
        )
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          for
            _     <- Client.get(s"$base/health")
            _     <- ZIO.succeed(Resources.gc())
            tcp0  <- ZIO.succeed(Resources.establishedTcp)
            heap0 <- ZIO.succeed(Resources.heapUsed)
            _     <- ZIO.attemptBlocking {
              val sock = Socket("127.0.0.1", port)
              sock.getOutputStream.write("GET /stream HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
              sock.getOutputStream.flush()
              sock.getInputStream.read()
              sock.close()
            }
            _     <- Client.get(s"$base/health")
            _     <- ZIO.succeed(Resources.gc())
            tcp1  <- ZIO.succeed(Resources.establishedTcp)
            heap1 <- ZIO.succeed(Resources.heapUsed)
          yield leaked(tcp0, tcp1, heap0, heap1)
        }
      ,
      test("three load cycles do not stair-step heap or TCP"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer(routes) { base =>
          val port = java.net.URI.create(base).getPort
          def burst =
            hits(port, 16) *>
              ZIO.succeed(Resources.gc()) *>
              ZIO.succeed((Resources.establishedTcp, Resources.heapUsed))
          for
            _             <- hits(port, 1)
            _             <- ZIO.succeed(Resources.gc())
            (tcp0, heap0) <- ZIO.succeed((Resources.establishedTcp, Resources.heapUsed))
            (tcp1, heap1) <- burst
            (tcp2, heap2) <- burst
            (tcp3, heap3) <- burst
          yield
            val heaps = List(heap1, heap2, heap3)
            val stair = heaps.sliding(2).forall {
              case a :: b :: Nil => b <= a + 8L * 1024 * 1024
              case _             => true
            }
            val tcpOk = List(tcp0, tcp1, tcp2, tcp3).flatten.sliding(2).forall {
              case a :: b :: Nil => b <= a + 2
              case _             => true
            }
            assertTrue(stair, tcpOk, heaps.forall(_ <= heap0 + 16L * 1024 * 1024))
        }
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds) @@ TestAspect.sequential

  private def cycle(n: Int, keepAlive: Boolean): ZIO[Any, Any, TestResult] =
    val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
    val header = if keepAlive then "" else "Connection: close\r\n"
    LiveServer(routes) { base =>
      val port = java.net.URI.create(base).getPort
      for
        _     <- Client.get(s"$base/health")
        _     <- ZIO.succeed(Resources.gc())
        tcp0  <- ZIO.succeed(Resources.establishedTcp)
        heap0 <- ZIO.succeed(Resources.heapUsed)
        pt0   <- ZIO.succeed(Resources.platformThreads)
        _     <- ZIO.foreachDiscard(0 until n) { _ =>
          ZIO.attemptBlocking {
            val sock = Socket("127.0.0.1", port)
            try
              sock.getOutputStream.write(s"GET /health HTTP/1.1\r\nHost: localhost\r\n$header\r\n".getBytes)
              sock.getOutputStream.flush()
              sock.getInputStream.readAllBytes()
            finally sock.close()
          }
        }
        _     <- ZIO.succeed(Resources.gc())
        tcp1  <- ZIO.succeed(Resources.establishedTcp)
        heap1 <- ZIO.succeed(Resources.heapUsed)
        pt1   <- ZIO.succeed(Resources.platformThreads)
      yield leaked(tcp0, tcp1, heap0, heap1) && assertTrue(pt1 <= pt0 + 8)
    }

  private def soakKeepAlive(n: Int): ZIO[Any, Any, TestResult] =
    val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
    LiveServer(routes) { base =>
      val port = java.net.URI.create(base).getPort
      for
        _     <- hits(port, 1)
        _     <- ZIO.succeed(Resources.gc())
        tcp0  <- ZIO.succeed(Resources.establishedTcp)
        heap0 <- ZIO.succeed(Resources.heapUsed)
        socks <- ZIO.foreachPar(0 until n) { _ =>
          ZIO.attemptBlocking {
            val sock = Socket("127.0.0.1", port)
            sock.getOutputStream.write("GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes)
            sock.getOutputStream.flush()
            sock.getInputStream.read()
            sock
          }
        }
        _     <- ZIO.foreachDiscard(socks)(s => ZIO.succeed(s.close()))
        _     <- ZIO.succeed(Resources.gc())
        tcp1  <- ZIO.succeed(Resources.establishedTcp)
        heap1 <- ZIO.succeed(Resources.heapUsed)
      yield leaked(tcp0, tcp1, heap0, heap1)
    }

  private def hits(port: Int, n: Int): Task[Unit] =
    ZIO.foreachDiscard(0 until n) { _ =>
      ZIO.attemptBlocking {
        val sock = Socket("127.0.0.1", port)
        try
          sock.getOutputStream.write(
            "GET /health HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".getBytes
          )
          sock.getOutputStream.flush()
          sock.getInputStream.readAllBytes()
        finally sock.close()
      }
    }

  private def leaked(
      tcp0: Option[Int],
      tcp1: Option[Int],
      heap0: Long,
      heap1: Long,
  ): TestResult =
    val tcpOk = (tcp0, tcp1) match
      case (Some(a), Some(b)) => b <= a + 2
      case _                  => true
    assertTrue(tcpOk, heap1 <= heap0 + 16L * 1024 * 1024)
end LeakSpec
