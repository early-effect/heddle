package heddle

import heddle.client.{ConnectionsPerHost, IdlePerHost}
import heddle.server.{Backlog, BodyBytes, ChunkSize, FrameSize, HeaderBytes, KeepAliveRequests, OutstandingFrames, Port}
import zio.*
import zio.test.*

object ServerConfigSpec extends ZIOSpecDefault:
  private def refused[A](value: Either[String, A], needle: String): Boolean =
    value.fold(_.contains(needle), _ => false)

  def spec = suite("Server.Config")(
    test("make accepts only a value in range"):
      check(Gen.int, Gen.int, Gen.int, Gen.int, Gen.long) { (port, chunk, frame, outstanding, body) =>
        assertTrue(
          Port.make(port).isRight == (port >= 0 && port <= 65535),
          ChunkSize.make(chunk).isRight == (chunk > 0),
          HeaderBytes.make(chunk).isRight == (chunk > 0),
          FrameSize.make(frame).isRight == (frame >= 16_384 && frame <= 16_777_215),
          OutstandingFrames.make(outstanding).isRight == (outstanding > 0),
          KeepAliveRequests.make(outstanding).isRight == (outstanding > 0),
          Backlog.make(port).isRight == (port >= 0),
          BodyBytes.make(body).isRight == (body >= 0),
          ConnectionsPerHost.make(outstanding).isRight == (outstanding > 0),
          IdlePerHost.make(port).isRight == (port >= 0),
        )
      }
    ,
    test("rejected values name the bound"):
      assertTrue(
        refused(Port.make(-1), "0 to 65535"),
        refused(Port.make(70_000), "0 to 65535"),
        refused(ChunkSize.make(0), "positive"),
        refused(KeepAliveRequests.make(0), "positive"),
        KeepAliveRequests.make(10).isRight,
        refused(FrameSize.make(100), "16384"),
        refused(BodyBytes.make(-1L), ">= 0"),
      )
    ,
    test("a missing keep-alive key is unlimited"):
      ZIO
        .config(Server.Config.descriptor)
        .withConfigProvider(ConfigProvider.fromMap(Map.empty))
        .map(c => assertTrue(c.maxRequestsPerConnection.isEmpty))
    ,
    test("keep-alive 0 fails to load and 10 loads"):
      val zero = ConfigProvider.fromMap(Map("heddle.server.maxRequestsPerConnection" -> "0"))
      val ten  = ConfigProvider.fromMap(Map("heddle.server.maxRequestsPerConnection" -> "10"))
      for
        bad <- ZIO.config(Server.Config.descriptor).withConfigProvider(zero).flip
        ok  <- ZIO.config(Server.Config.descriptor).withConfigProvider(ten)
      yield assertTrue(
        bad.toString.contains("positive"),
        ok.maxRequestsPerConnection == Some(KeepAliveRequests(10)),
      )
    ,
    test("a frame size below 16384 fails to load"):
      val provider = ConfigProvider.fromMap(Map("heddle.server.http2.maxFrameSize" -> "100"))
      ZIO.config(Server.Config.descriptor).withConfigProvider(provider).flip.map { err =>
        assertTrue(err.toString.contains("16384"))
      }
    ,
    test("a size that does not fit an Int saturates instead of wrapping"):
      check(Gen.long) { n =>
        val expected = if n > Int.MaxValue then Int.MaxValue else if n < Int.MinValue then Int.MinValue else n.toInt
        assertTrue(BytesLength(n).toInt == expected)
      },
  ) @@ TestAspect.timeout(60.seconds)
end ServerConfigSpec
