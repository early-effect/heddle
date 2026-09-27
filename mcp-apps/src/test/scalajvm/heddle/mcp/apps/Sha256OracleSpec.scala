package heddle.mcp.apps

import java.security.MessageDigest
import zio.*
import zio.test.*

object Sha256OracleSpec extends ZIOSpecDefault:
  def spec = suite("SHA-256 oracle")(
    test("agrees with the JDK's MessageDigest for any bytes"):
      check(Gen.int(0, 3000).flatMap(n => Gen.fromZIO(Random.nextBytes(n)))) { bytes =>
        val jdk = Chunk.fromArray(MessageDigest.getInstance("SHA-256").digest(bytes.toArray))
        assertTrue(Sha256.digest(bytes) == jdk)
      }
  ) @@ TestAspect.timeout(60.seconds)
end Sha256OracleSpec
