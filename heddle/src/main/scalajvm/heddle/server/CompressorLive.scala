package heddle.server

import java.io.ByteArrayOutputStream
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import heddle.error.{HttpError, WireError}
import heddle.http.ContentEncoding
import zio.Chunk
import zio.stream.ZStream

private[server] object CompressorLive:
  val gzip: Compressor = GzipCompressor

  def gunzip(bytes: Chunk[Byte], limit: Long): Either[HttpError, Chunk[Byte]] =
    try
      val in = GZIPInputStream(java.io.ByteArrayInputStream(bytes.toArray))
      try
        val out   = ByteArrayOutputStream(math.min(limit, 64L * 1024).toInt.max(64))
        val buf   = new Array[Byte](8192)
        var total = 0L
        var n     = in.read(buf)
        while n >= 0 && total <= limit do
          total += n
          if total <= limit then out.write(buf, 0, n)
          n = in.read(buf)
        if total > limit then Left(HttpError.BodyTooLarge) else Right(Chunk.fromArray(out.toByteArray))
      finally in.close()
      end try
    catch
      case e: java.io.IOException =>
        Left(HttpError.Malformed(WireError.Undecodable(ContentEncoding.Gzip, String.valueOf(e.getMessage))))
end CompressorLive

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
