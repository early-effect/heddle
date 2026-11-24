package heddle

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import zio.*
import zio.stream.ZStream
import zio.test.*

object NativeGzipFlushSpec extends ZIOSpecDefault:
  private val event = Chunk.fromArray("data: hello\n\n".getBytes(UTF_8))

  private enum GzipError:
    case Io(cause: IOException)
    case Compression(cause: Throwable)

  private def io[A](operation: => A): IO[GzipError, A] =
    ZIO.attemptBlockingIO(operation).mapError(GzipError.Io.apply)

  def spec =
    val suiteSpec = suite("Native gzip flush")(
      test("javalib sync flush emits decodable payload bytes before close") {
        ZIO.scoped:
          for
            output <- ZIO.succeed(ByteArrayOutputStream())
            gzip   <- ZIO.acquireRelease(io(GZIPOutputStream(output, 512, true)))(stream => io(stream.close()).ignore)
            header <- ZIO.succeed(output.size())
            bytes  <- io {
              gzip.write(event.toArray)
              gzip.flush()
              Chunk.fromArray(output.toByteArray)
            }
            input <- ZIO.acquireRelease(io(GZIPInputStream(ByteArrayInputStream(bytes.toArray))))(stream =>
              io(stream.close()).ignore
            )
            decoded <- io(Chunk.fromArray(input.readNBytes(event.length)))
          yield assertTrue(header == 10, bytes.length > header, decoded == event)
      },
      test("heddle's gzip stream carries compressed bytes in the chunk for its first input") {
        Compressor.gzip
          .stream(ZStream.fromChunk(event) ++ ZStream.never)
          .chunks
          .runHead
          .mapError(GzipError.Compression.apply)
          .map { first =>
            assertTrue(first.exists(_.length > 10))
          }
      },
    )
    suiteSpec @@ TestAspect.timeout(20.seconds)
end NativeGzipFlushSpec
