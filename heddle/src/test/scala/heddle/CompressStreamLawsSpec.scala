package heddle

import zio.*
import zio.stream.ZStream
import zio.test.*

object CompressStreamLawsSpec extends ZIOSpecDefault:
  private val pieces: Gen[Any, List[Chunk[Byte]]] =
    Gen.listOfBounded(0, 8)(ByteGens.upTo(512))

  def spec = suite("Streaming gzip")(
    test("gunzip of a streamed gzip is the input, however it was chunked"):
      check(pieces) { ps =>
        Compressor.gzip
          .stream(ZStream.fromIterable(ps).flattenChunks)
          .runCollect
          .map(gz => assertTrue(Compressor.gunzip(gz, 1.M) == Right(ps.foldLeft(Chunk.empty[Byte])(_ ++ _))))
      }
    ,
    test("bytes leave before the input ends, so a compressed event stream is live"):
      val never = ZStream.fromChunk(Chunk.fromArray("data: hello\n\n".getBytes)) ++ ZStream.never
      Compressor.gzip.stream(never).take(12).runCollect.map(first => assertTrue(first.length == 12)),
  ) @@ TestAspect.timeout(20.seconds)
end CompressStreamLawsSpec
