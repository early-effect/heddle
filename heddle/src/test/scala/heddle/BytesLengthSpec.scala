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
      test("Server.Config defaults use K and M"):
        val c = Server.Config.default
        assertTrue(c.maxHeaderBytes == 64.K, c.maxBodyBytes == 10.M, c.chunkSize == 8.K)
      ,
      test("64.K minus 1.B is the HTTP/2 default window"):
        assertTrue((64.K - 1.B).toInt == 65535),
    )
end BytesLengthSpec
