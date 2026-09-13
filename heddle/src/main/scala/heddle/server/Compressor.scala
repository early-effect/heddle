package heddle.server

import java.io.ByteArrayOutputStream
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import heddle.http.ContentEncoding
import zio.Chunk
import zio.stream.ZStream

trait Compressor:
  def encoding: ContentEncoding
  def compress(bytes: Chunk[Byte]): Chunk[Byte]
  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte]

object Compressor:
  val gzip: Compressor = GzipCompressor

  def gunzip(bytes: Chunk[Byte]): Chunk[Byte] =
    val in = GZIPInputStream(java.io.ByteArrayInputStream(bytes.toArray))
    try Chunk.fromArray(in.readAllBytes())
    finally in.close()

private object GzipCompressor extends Compressor:
  def encoding: ContentEncoding = ContentEncoding.Gzip

  def compress(bytes: Chunk[Byte]): Chunk[Byte] =
    val bos = ByteArrayOutputStream(bytes.length.max(64))
    val gz  = GZIPOutputStream(bos)
    gz.write(bytes.toArray)
    gz.finish()
    gz.close()
    Chunk.fromArray(bos.toByteArray)

  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap {
      zio.ZIO.succeed {
        val bos = ByteArrayOutputStream(512)
        val gz  = GZIPOutputStream(bos, 512, true)
        in.mapChunks { c =>
          if c.nonEmpty then gz.write(c.toArray)
          gz.flush()
          val out = bos.toByteArray
          bos.reset()
          Chunk.fromArray(out)
        } ++ ZStream.fromZIO {
          zio.ZIO.succeed {
            gz.finish()
            val out = bos.toByteArray
            gz.close()
            Chunk.fromArray(out)
          }
        }.flattenChunks
      }
    }
end GzipCompressor
