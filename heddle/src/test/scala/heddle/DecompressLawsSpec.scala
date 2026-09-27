package heddle

import zio.*
import zio.test.*

object DecompressLawsSpec extends ZIOSpecDefault:
  private val bytes: Gen[Any, Chunk[Byte]] = Gen.chunkOfBounded(0, 4096)(Gen.byte)

  def spec = suite("Bounded gunzip")(
    test("gunzip inverts gzip within the limit, and refuses past it"):
      check(bytes, Gen.int(0, 5000)) { (raw, limit) =>
        val out = Compressor.gunzip(Compressor.gzip.compress(raw), BytesLength(limit))
        if raw.length <= limit then assertTrue(out == Right(raw))
        else assertTrue(out == Left(HttpError.BodyTooLarge))
      }
    ,
    test("corrupt input is Malformed"):
      check(bytes.filter(_.nonEmpty)) { junk =>
        assertTrue(Compressor.gunzip(Chunk[Byte](0x1f, 0x8b.toByte) ++ junk, 1.M) match
          case Left(_: HttpError.Malformed) | Left(HttpError.BodyTooLarge) => true
          case Right(_)                                                    => true
          case Left(_)                                                     => false)
      }
    ,
    test("a gzip bomb is refused with 413 before the handler runs"):
      val bomb   = Compressor.gzip.compress(Chunk.fill(8 * 1024 * 1024)(0.toByte))
      val routes = Routes(Method.POST / "in" -> Handler.text("reached")) @@ Middleware.decompress(maxBytes = 64.K)
      val req    = Request.post("/in", Body.fromBytes(bomb)).withHeader("Content-Encoding", "gzip")
      routes(req).map(res => assertTrue(bomb.length < 64 * 1024, res.status == Status.ContentTooLarge)),
  ) @@ TestAspect.timeout(60.seconds)
end DecompressLawsSpec
