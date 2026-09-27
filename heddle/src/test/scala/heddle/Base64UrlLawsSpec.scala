package heddle

import heddle.crypto.{Base64Error, Base64Url}
import zio.*
import zio.test.*

object Base64UrlLawsSpec extends ZIOSpecDefault:
  private val alphabet = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '-' :+ '_'
  private val encoded  = Gen.stringBounded(0, 40)(Gen.elements(alphabet*))

  def spec = suite("base64url laws")(
    test("decode inverts encode, with or without padding"):
      check(ByteGens.upTo(64)) { bytes =>
        val bare   = Base64Url.encode(bytes)
        val padded = bare + "=" * ((4 - bare.length % 4) % 4)
        assertTrue(Base64Url.decode(bare) == Right(bytes), Base64Url.decode(padded) == Right(bytes))
      }
    ,
    test("any run of the alphabet decodes to whole bytes, unless one character is left over"):
      check(encoded) { s =>
        if s.length % 4 == 1 then assertTrue(Base64Url.decode(s) == Left(Base64Error.BadLength(s.length)))
        else assertTrue(Base64Url.decode(s).map(_.length) == Right(s.length * 3 / 4))
      }
    ,
    test("a character outside the alphabet is named where it first appears"):
      check(encoded, encoded, Gen.elements('+', '/', ' ', '.', 'é', '=')) { (before, after, bad) =>
        assertTrue(
          Base64Url.decode(s"$before$bad${after}A") == Left(Base64Error.BadCharacter(bad, before.length))
        )
      },
  ) @@ TestAspect.timeout(60.seconds)
end Base64UrlLawsSpec
