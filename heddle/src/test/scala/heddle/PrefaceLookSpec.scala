package heddle

import java.nio.charset.StandardCharsets
import heddle.internal.engine.PrefaceLook
import heddle.internal.h2.H2Frame
import zio.test.*

object PrefaceLookSpec extends ZIOSpecDefault:
  private val preface = H2Frame.Preface
  private val cap     = 16 * 1024

  private def look(raw: Array[Byte], max: Int = cap): PrefaceLook =
    PrefaceLook.classify(raw, 0, raw.length, preface, max)

  def spec = suite("preface classification")(
    test("HELLO with a blank line is HTTP/1"):
      assertTrue(look("HELLO\r\n\r\n".getBytes(StandardCharsets.US_ASCII)) == PrefaceLook.Http1)
    ,
    test("the first 16 bytes of the preface still need more"):
      assertTrue(look(preface.take(16)) == PrefaceLook.NeedMore)
    ,
    test("a preface prefix that already contains a blank line still needs more"):
      // PRI * HTTP/2.0\r\n\r\n is 18 bytes and a proper prefix. The blank line is not the end of an HTTP/1 message.
      assertTrue(look(preface.take(18)) == PrefaceLook.NeedMore)
    ,
    test("the 24-byte preface is HTTP/2"):
      assertTrue(look(preface) == PrefaceLook.Http2)
    ,
    test("a finished GET is HTTP/1"):
      val raw = "GET /plaintext HTTP/1.1\r\nHost: localhost\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
      assertTrue(look(raw) == PrefaceLook.Http1)
    ,
    test("a diverged block at the cap with no blank line is too large"):
      assertTrue(look(Array.fill(32)('A'.toByte), max = 32) == PrefaceLook.HeadersTooLarge)
    ,
    test("an empty buffer needs more"):
      assertTrue(look(Array.emptyByteArray) == PrefaceLook.NeedMore),
  )
end PrefaceLookSpec
