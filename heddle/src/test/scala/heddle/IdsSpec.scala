package heddle

import heddle.internal.Ids
import zio.test.*

object IdsSpec extends ZIOSpecDefault:
  def spec =
    suite("Ids")(
      test("uuid is RFC 4122 version 4"):
        val id = Ids.uuid()
        assertTrue(
          ((id.getMostSignificantBits >>> 12) & 0xf) == 4L,
          ((id.getLeastSignificantBits >>> 62) & 0x3) == 2L,
        )
      ,
      test("successive ids differ"):
        val a = Ids.uuid()
        val b = Ids.uuid()
        assertTrue(a != b),
    )
end IdsSpec
