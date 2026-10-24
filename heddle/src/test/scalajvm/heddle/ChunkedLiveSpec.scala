package heddle

import heddle.server.{BodyBytes, HeaderBytes}
import zio.*
import zio.stream.ZStream
import zio.test.*

object ChunkedLiveSpec extends ZIOSpecDefault:
  private val payload = Chunk.fromArray(Array.fill[Byte](64 * 1024)(42))
  private val routes  = Routes(
    Method.GET / "body" -> Handler.succeed(Response(Status.Ok, Body.stream(ZStream.fromChunk(payload))))
  )
  private val client = ZLayer.succeed(Client.Config.default.copy(maxHeaderBytes = HeaderBytes(128))) >>> Client.layer

  def spec = suite("chunked loopback")(
    test("streaming receives one 64 KiB wire chunk through a 128-byte client buffer"):
      LiveServer(routes) { base =>
        ZIO
          .scoped {
            Client
              .streaming(Request.get(s"$base/body"))
              .flatMap(_.body.toStream.mapError(ClientError.BodyFailed(_)).runCollect)
          }
          .provide(client)
          .map(got => assertTrue(got == payload))
      }
    ,
    test("batched receives one 64 KiB wire chunk through a 128-byte client buffer"):
      LiveServer(routes) { base =>
        Client
          .batched(Request.get(s"$base/body"))
          .provide(client)
          .map(res => assertTrue(res.body.strict.contains(payload)))
      }
    ,
    test("identity close-delimited streaming preserves every byte of a 1 MiB body"):
      val size      = 1024L * 1024
      val blockSize = 64 * 1024
      val block     = Chunk.fromArray(Array.tabulate[Byte](blockSize)(i => (i % 251).toByte))
      val body      = Body.stream(ZStream.repeatZIOChunk(ZIO.succeed(block)).take(size), length = Some(size))
      val response  = Response(Status.Ok, body)
        .withHeader(HeaderName.TransferEncoding, "identity")
        .withHeader(HeaderName.Connection, "close")
      val routes = Routes.fromHandler(Handler.succeed(response))
      val client =
        ZLayer.succeed(Client.Config.default.copy(maxBodyBytes = BodyBytes(256L * 1024 * 1024))) >>> Client.layer
      LiveServer(routes) { base =>
        ZIO
          .scoped {
            Client.streaming(Request.get(s"$base/")).flatMap { found =>
              found.body.toStream.mapError(ClientError.BodyFailed(_)).runFold((0L, 0L)) { (counts, byte) =>
                val expected = ((counts._1 % blockSize) % 251).toByte
                (counts._1 + 1, counts._2 + (if byte == expected then 0L else 1L))
              }
            }
          }
          .provide(client)
          .map(counts => assertTrue(counts == (size, 0L)))
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)
end ChunkedLiveSpec
