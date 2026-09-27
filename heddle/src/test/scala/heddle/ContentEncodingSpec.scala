package heddle

import zio.*
import zio.test.*

object ContentEncodingSpec extends ZIOSpecDefault:
  private val ours = Chunk(ContentEncoding.Brotli, ContentEncoding.Gzip)

  def spec = suite("content coding negotiation")(
    test("the highest quality coding we have wins, ties going to our preference"):
      assertTrue(
        ContentEncoding.negotiate(Some("gzip;q=0.8, br;q=0.9"), ours).contains(ContentEncoding.Brotli),
        ContentEncoding.negotiate(Some("gzip;q=0.9, br;q=0.8"), ours).contains(ContentEncoding.Gzip),
        ContentEncoding.negotiate(Some("gzip, br"), ours).contains(ContentEncoding.Brotli),
        ContentEncoding.negotiate(Some("*"), ours).contains(ContentEncoding.Brotli),
      )
    ,
    test("q=0 refuses a coding even under *, and an absent header or a stranger's coding is identity"):
      assertTrue(
        ContentEncoding.negotiate(Some("*, br;q=0"), ours).contains(ContentEncoding.Gzip),
        ContentEncoding.negotiate(Some("br;q=0, gzip;q=0"), ours).isEmpty,
        ContentEncoding.negotiate(Some("zstd"), ours).isEmpty,
        ContentEncoding.negotiate(None, ours).isEmpty,
      )
    ,
    test("a quality outside 0 to 1, or not a number, drops only that member"):
      assertTrue(
        ContentEncoding.parseAccept("gzip;q=2, br;q=abc, deflate;q=0.5") ==
          Chunk(AcceptEncoding(EncodingRange.Only(ContentEncoding.Deflate), 0.5))
      )
    ,
    test("an unknown coding keeps its name, and every coding renders back to its token"):
      check(Gen.stringBounded(1, 10)(Gen.alphaChar).map(_.toLowerCase)) { token =>
        val c = ContentEncoding.fromToken(token)
        assertTrue(c.token == token || (token == "x-gzip" && c == ContentEncoding.Gzip))
      },
  ) @@ TestAspect.timeout(60.seconds)
end ContentEncodingSpec
