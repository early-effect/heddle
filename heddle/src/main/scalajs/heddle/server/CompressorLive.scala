package heddle.server

import heddle.http.ContentEncoding
import heddle.internal.node.Zlib
import scala.scalajs.js.typedarray.Uint8Array
import zio.Chunk
import zio.stream.ZStream

private[server] object CompressorLive:
  val gzip: Compressor = NodeGzip

  def gunzip(bytes: Chunk[Byte]): Chunk[Byte] =
    fromU8(Zlib.gunzipSync(toU8(bytes)))

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

  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap(in.runCollect.map(c => ZStream.fromChunk(compress(c))))
end NodeGzip
