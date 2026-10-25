package heddle

import BytesLength.*
import zio.test.*

object BytesLengthSpec extends ZIOSpecDefault:
  def spec =
    suite("BytesLength")(
      test("K and M are 1024-based"):
        assertTrue(
          64.K.toLong == 65536L,
          10.M.toLong == 10L * 1024 * 1024,
          1.G.toLong == 1024L * 1024 * 1024,
          8.B.toLong == 8L,
        )
      ,
      test("server caps match the K and M sizes"):
        val c  = Server.Config.default
        val h2 = c.http2Config
        assertTrue(
          c.maxHeaderBytes.toInt == 16.K.toInt,
          c.maxBodyBytes.toLong == 10.M.toLong,
          c.chunkSize.toInt == 8.K.toInt,
          h2.initialWindowSize.toInt == (64.K - 1.B).toInt,
          h2.maxFrameSize.toInt == 16.K.toInt,
          h2.maxHeaderListSize.toInt == 8.K.toInt,
        )
      ,
      test("64.K minus 1.B is the HTTP/2 default window"):
        assertTrue((64.K - 1.B).toInt == 65535),
    ) @@ TestAspect.timeout(zio.Duration.fromSeconds(60))
