package heddle

import java.net.{URLDecoder, URLEncoder}
import zio.test.*

object UrlEncodingJvmSpec extends ZIOSpecDefault:
  def spec =
    suite("UrlEncoding JVM oracle")(
      test("encodeForm matches URLEncoder UTF-8"):
        val samples = List("a b", "1&2", "*~", "é", "€", "a+b", "q=1", "")
        assertTrue(
          samples.forall { s =>
            UrlEncoding.encodeForm(s) == URLEncoder.encode(s, "UTF-8")
          }
        )
      ,
      test("decodeForm matches URLDecoder UTF-8"):
        val samples = List("a+b", "1%262", "*%7E", "%C3%A9", "%E2%82%AC", "a%2Bb")
        assertTrue(
          samples.forall { s =>
            UrlEncoding.decodeForm(s) == URLDecoder.decode(s, "UTF-8")
          }
        ),
    )
end UrlEncodingJvmSpec
