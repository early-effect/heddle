package heddle

import java.io.ByteArrayOutputStream
import java.util.zip.GZIPOutputStream
import zio.*
import zio.stream.ZStream
import zio.test.*

object NativeGzipFlushSpec extends ZIOSpecDefault:
  private val event = Chunk.fromArray("data: hello\n\n".getBytes)

  def spec = suite("Native gzip flush")(
    test("javalib's sync flush still emits nothing (Scala Native 0.5.12); when this fails, it is fixed upstream") {
      val bos    = ByteArrayOutputStream()
      val gz     = GZIPOutputStream(bos, 512, true)
      val header = bos.size()
      gz.write(event.toArray)
      gz.flush()
      assertTrue(header == 10, bos.size() == header)
    },
    test("heddle's gzip stream carries compressed bytes in the chunk for its first input") {
      Compressor.gzip.stream(ZStream.fromChunk(event) ++ ZStream.never).chunks.runHead.map { first =>
        assertTrue(first.exists(_.length > 10))
      }
    },
  ) @@ TestAspect.timeout(20.seconds)
end NativeGzipFlushSpec
