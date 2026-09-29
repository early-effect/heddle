package heddle

import heddle.internal.Ids
import zio.*
import zio.test.*

object IdsSpec extends ZIOSpecDefault:
  def spec =
    suite("Ids")(
      test("uuid is RFC 9562 version 4, variant 10"):
        Ids.uuid.map { id =>
          assertTrue(
            ((id.getMostSignificantBits >>> 12) & 0xf) == 4L,
            ((id.getLeastSignificantBits >>> 62) & 0x3) == 2L,
          )
        }
      ,
      test("a thousand draws never repeat, and tokens are 32 hex characters"):
        (ZIO.collectAll(Chunk.fill(1000)(Ids.uuid)) <*> Ids.token).map { (ids, token) =>
          assertTrue(
            ids.distinct.length == 1000,
            token.length == 32,
            token.forall(c => c.isDigit || ('a' to 'f').contains(c)),
          )
        }
      ,
      test("a draw of any size is filled to its end, past the most one platform call gives") {
        check(Gen.int(0, 200000)) { n =>
          Ids.bytes(n).map { b =>
            // The chance that 4096 random bytes are all zero is 2^-32768; a tail left unfilled is all zero.
            val tail = b.takeRight(4096)
            assertTrue(b.length == n, tail.isEmpty || tail.exists(_ != 0))
          }
        }
      } @@ TestAspect.samples(20),
    ) @@ TestAspect.timeout(60.seconds)
end IdsSpec
