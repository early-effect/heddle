package heddle

import heddle.internal.duplex.NativeListener
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.scalanative.meta.LinktimeInfo
import zio.*
import zio.test.*

object NativeAcceptSpec extends ZIOSpecDefault:
  def spec =
    suite("Native accept")(
      test("runtime is multithreaded"):
        assertTrue(LinktimeInfo.isMultithreadingEnabled)
      ,
      test("two blocking sleeps complete together"):
        val a = ZIO.attemptBlockingInterrupt(Thread.sleep(80))
        val b = ZIO.attemptBlockingInterrupt(Thread.sleep(80))
        a.zipPar(b).timed.map { (d, _) =>
          assertTrue(d.toMillis < 150)
        }
      ,
      test("async accept completes after a client connects"):
        ZIO.scoped {
          for
            listener <- NativeListener.bind(local)
            port     <- listener.localPort
            parked   <- listener.accept.fork
            _        <- ZIO.attempt(heddle.internal.posix.Net.connect("127.0.0.1", port)).flatMap { fd =>
              heddle.internal.posix.AsyncFd.writable(fd) *>
                ZIO.succeed(heddle.internal.posix.Net.close(fd))
            }
            conn <- parked.join
            _    <- conn.close
          yield assertTrue(port > 0)
        }
      ,
      test("scope close unblocks a listening accept"):
        val opened =
          ZIO.scoped {
            NativeListener.bind(local).flatMap { listener =>
              listener.localPort.delay(20.millis)
            }
          }
        opened.map(port => assertTrue(port > 0))
      ,
      test("one accept then Client GET"):
        ZIO.scoped {
          for
            listener <- NativeListener.bind(local)
            port     <- listener.localPort
            server   <- serveOnce(listener).fork
            res      <- Client.get(s"http://127.0.0.1:$port/health")
            bytes    <- res.body.collect
            _        <- server.join
          yield assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
        }
      ,
      test("accept loop then Client GET"):
        ZIO.scoped {
          for
            listener <- NativeListener.bind(local)
            port     <- listener.localPort
            loop = listener.accept.flatMap(c => serveConn(c).forkDaemon.as(())).forever
            server <- loop.fork
            res    <- Client.get(s"http://127.0.0.1:$port/health")
            bytes  <- res.body.collect
            _      <- server.interrupt
          yield assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(8.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = 0)

  private def serveOnce(listener: heddle.internal.duplex.Listener): Task[Unit] =
    listener.accept.flatMap(serveConn)

  private def serveConn(conn: heddle.internal.duplex.ByteConn): Task[Unit] =
    val buf = ByteBuffer.allocate(1024)
    conn
      .read(buf)
      .mapError(e => java.io.IOException(e.message))
      .flatMap { _ =>
        val res = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
        conn.write(Chunk.fromArray(res.getBytes(StandardCharsets.US_ASCII)))
      }
      .ensuring(conn.close)
  end serveConn
end NativeAcceptSpec
