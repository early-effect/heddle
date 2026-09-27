package heddle.server

import heddle.error.{HttpError, WireError}
import heddle.http.ContentEncoding
import heddle.internal.Crc32
import heddle.internal.node.{Zlib, ZlibFlush, ZlibOptions}
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import zio.{Chunk, Ref}
import zio.stream.ZStream

private[server] object CompressorLive:
  val gzip: Compressor = NodeGzip

  def gunzip(bytes: Chunk[Byte], limit: Long): Either[HttpError, Chunk[Byte]] =
    val cap = math.min(limit + 1, Int.MaxValue.toLong).toDouble
    try
      val out = fromU8(Zlib.gunzipSync(toU8(bytes), ZlibOptions(maxOutputLength = cap)))
      if out.length.toLong > limit then Left(HttpError.BodyTooLarge) else Right(out)
    catch
      case js.JavaScriptException(e: js.Error) if e.name == "RangeError" => Left(HttpError.BodyTooLarge)
      case js.JavaScriptException(e: js.Error)                           =>
        Left(HttpError.Malformed(WireError.Undecodable(ContentEncoding.Gzip, e.message)))

  private[server] def toU8(bytes: Chunk[Byte]): Uint8Array =
    val a = Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      a(i) = (bytes(i) & 0xff).toShort
      i += 1
    a

  private[server] def fromU8(a: Uint8Array): Chunk[Byte] =
    val n   = a.length
    val out = Array.ofDim[Byte](n)
    var i   = 0
    while i < n do
      out(i) = a(i).toByte
      i += 1
    Chunk.fromArray(out)
end CompressorLive

private object NodeGzip extends Compressor:
  def encoding: ContentEncoding = ContentEncoding.Gzip

  def compress(bytes: Chunk[Byte]): Chunk[Byte] =
    CompressorLive.fromU8(Zlib.gzipSync(CompressorLive.toU8(bytes)))

  /** Emits as the input arrives. Each chunk is raw deflate ending in a sync flush (no final block), so the pieces
    * concatenate into one deflate stream; gzip framing and the CRC-32 trailer wrap them.
    */
  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap(Ref.make((0, 0L)).map { seen =>
      val body = in.chunks.filter(_.nonEmpty).mapZIO { c =>
        seen.update((crc, size) => (Crc32.update(crc, c), size + c.length)).as(deflate(c, SyncFlush))
      }
      val tail = ZStream.fromZIO(seen.get.map((crc, size) => deflate(Chunk.empty, Finish) ++ trailer(crc, size)))
      (ZStream.succeed(Header) ++ body ++ tail).flattenChunks
    })

  private val SyncFlush = 2
  private val Finish    = 4

  private val Header: Chunk[Byte] = Chunk[Byte](0x1f, 0x8b.toByte, 8, 0, 0, 0, 0, 0, 0, 0xff.toByte)

  private def deflate(bytes: Chunk[Byte], flush: Int): Chunk[Byte] =
    CompressorLive.fromU8(Zlib.deflateRawSync(CompressorLive.toU8(bytes), ZlibFlush(finishFlush = flush)))

  private def trailer(crc: Int, size: Long): Chunk[Byte] =
    Chunk.fromIterable(
      (0 until 4).map(i => (crc >>> (8 * i)).toByte) ++ (0 until 4).map(i => (size >>> (8 * i)).toByte)
    )
end NodeGzip
