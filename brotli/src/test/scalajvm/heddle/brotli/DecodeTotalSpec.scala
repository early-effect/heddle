package heddle.brotli

import heddle.error.{HttpError, WireError}
import heddle.http.ContentEncoding
import zio.*
import zio.test.*

object DecodeTotalSpec extends ZIOSpecDefault:
  /** A real stream with one bit flipped, or cut short: input that reaches the trees, context maps, and references. */
  private val damaged: Gen[Any, Array[Byte]] =
    (BrotliGens.bytes <*> Gen.int(0, Int.MaxValue) <*> Gen.int(0, 7) <*> Gen.boolean).map { (raw, at, bit, cut) =>
      val enc = Brotli.encodeArray(raw)
      if cut then enc.take(at % enc.length)
      else enc.updated(at     % enc.length, (enc(at % enc.length) ^ (1 << bit)).toByte)
    }

  def spec = suite("brotli decode is total")(
    test("damaged and arbitrary input decodes or is Undecodable, never a throw"):
      check(Gen.oneOf(damaged, BrotliGens.bytes)) { input =>
        Brotli.decode(Chunk.fromArray(input)) match
          case Right(_)                                                                    => assertCompletes
          case Left(HttpError.Malformed(WireError.Undecodable(ContentEncoding.Brotli, _))) => assertCompletes
          case Left(HttpError.BodyTooLarge)                                                => assertCompletes
          case Left(other) => assertNever(s"unexpected $other")
      }
  ) @@ TestAspect.samples(500) @@ TestAspect.timeout(120.seconds)
end DecodeTotalSpec
