package heddle.brotli

import zio.*
import zio.test.*

object HuffmanSpec extends ZIOSpecDefault:
  def spec =
    suite("Huffman")(
      test("RFC example lengths (2,1,3,3) assign codes 2,0,6,7"):
        val len = Array(2, 1, 3, 3)
        val c   = Huffman.codes(len)
        assertTrue(c.toSeq == Seq(2, 0, 6, 7))
      ,
      test("simple prefix for one symbol writes 2+2+alphabet bits"):
        val len = Array.fill(256)(0)
        len('n') = 1
        val w = BitWriter()
        Huffman.writePrefixCode(w, len, 8)
        val bits = w.finish()
        assertTrue(bits.nonEmpty, (bits(0) & 3) == 1)
      ,
      test("empty alphabet is written as a two-symbol simple prefix"):
        val len = Array.fill(64)(0)
        val w   = BitWriter()
        Huffman.writePrefixCode(w, len, 6)
        val bits = w.finish()
        assertTrue(bits.nonEmpty, (bits(0) & 3) == 1, ((bits(0) >> 2) & 3) == 1, len(0) == 1, len(1) == 1)
      ,
      test("RFC example codes are prefix-free"):
        val len = Array(2, 1, 3, 3)
        assertTrue(Huffman.prefixFree(len, Huffman.codes(len)))
      ,
      test("lengths from frequencies yield prefix-free codes"):
        check(Gen.listOfBounded(2, 48)(Gen.int(1, 200))) { freqs =>
          val freq  = freqs.toArray
          val len   = Huffman.lengths(freq, 15)
          val codes = Huffman.codes(len)
          assertTrue(Huffman.prefixFree(len, codes))
        },
    ) @@ TestAspect.timeout(30.seconds)
end HuffmanSpec
