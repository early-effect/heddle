package heddle

import heddle.internal.Ascii
import zio.*
import zio.test.*

object AsciiDigitsSpec extends ZIOSpecDefault:
  private val nonNegative = Gen.long(0, Long.MaxValue)
  private val digits      = Gen.stringBounded(1, 24)(Gen.numericChar)
  private val hexDigits   = Gen.stringBounded(1, 20)(Gen.elements(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')*))

  def spec = suite("Ascii digits")(
    test("decimal and hex read back every non-negative Long, in either hex case"):
      check(nonNegative) { n =>
        val h = java.lang.Long.toHexString(n)
        assertTrue(
          Ascii.decimal(n.toString).contains(n),
          Ascii.hex(h).contains(n),
          Ascii.hex(h.toUpperCase).contains(n),
        )
      }
    ,
    test("a run of digits reads as its value, or not at all once it overflows"):
      check(digits, hexDigits) { (d, h) =>
        val dv = BigInt(d)
        val hv = BigInt(h, 16)
        assertTrue(
          Ascii.decimal(d) == Option.when(dv <= Long.MaxValue)(dv.toLong),
          Ascii.hex(h) == Option.when(hv <= Long.MaxValue)(hv.toLong),
        )
      }
    ,
    test("signs, whitespace, prefixes, and non-ASCII digits are refused"):
      check(nonNegative) { n =>
        val refused = List(s"+$n", s"-$n", s" $n", s"$n ", s"0x$n", s"${n}_", "", "\u0663", s"$n\u0663")
        assertTrue(refused.forall(Ascii.decimal(_).isEmpty), refused.forall(Ascii.hex(_).isEmpty))
      },
  ) @@ TestAspect.timeout(60.seconds)
end AsciiDigitsSpec
