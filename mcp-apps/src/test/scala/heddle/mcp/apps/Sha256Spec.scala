package heddle.mcp.apps

import java.nio.charset.StandardCharsets
import zio.*
import zio.test.*

object Sha256Spec extends ZIOSpecDefault:
  private def of(s: String): String = Sha256.hex(Chunk.fromArray(s.getBytes(StandardCharsets.UTF_8)))

  def spec = suite("SHA-256")(
    test("matches the FIPS 180-4 vectors"):
      assertTrue(
        of("") == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        of("abc") == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") ==
          "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1",
        of("a" * 1000000) == "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0",
      )
    ,
    test("every block boundary pads correctly"):
      check(Gen.int(0, 200))(n => assertTrue(of("x" * n).length == 64)),
  ) @@ TestAspect.timeout(60.seconds)
end Sha256Spec
