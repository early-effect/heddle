package heddle.crypto

import java.nio.charset.StandardCharsets
import zio.Chunk
import zio.durationInt
import zio.test.*

object CryptoSpec extends ZIOSpecDefault:
  def spec =
    suite("Digest and Rsa")(
      test("sha256 empty vector"):
        Digest.sha256(Chunk.empty).map { got =>
          assertTrue(hex(got) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        }
      ,
      test("sha256 abc vector"):
        Digest.sha256(utf8("abc")).map { got =>
          assertTrue(hex(got) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        }
      ,
      test("RS256 sign and verify"):
        for
          key <- Rsa.generate(2048)
          msg = utf8("rs256-payload")
          sig <- Rsa.signSha256(key, msg)
          ok  <- Rsa.verifySha256(key.public, msg, sig)
          bad <- Rsa.verifySha256(key.public, utf8("other"), sig)
        yield assertTrue(ok, !bad, sig.nonEmpty),
    ).provide(Digest.live, Rsa.live) @@ TestAspect.timeout(10.seconds)

  private def utf8(s: String): Chunk[Byte] =
    Chunk.fromArray(s.getBytes(StandardCharsets.UTF_8))

  private def hex(bytes: Chunk[Byte]): String =
    val digits = "0123456789abcdef"
    val out    = Array.ofDim[Char](bytes.length * 2)
    var i      = 0
    while i < bytes.length do
      val v = bytes(i) & 0xff
      out(i * 2) = digits.charAt(v >>> 4)
      out(i * 2 + 1) = digits.charAt(v & 0x0f)
      i += 1
    String(out)
  end hex
end CryptoSpec
