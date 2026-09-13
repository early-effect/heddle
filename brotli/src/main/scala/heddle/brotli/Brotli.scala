package heddle.brotli

import heddle.http.ContentEncoding
import heddle.server.{Compressor, Decompressor}
import zio.Chunk
import zio.stream.ZStream

/** RFC 7932 encoder: greedy LZ77 plus Huffman-coded metablocks. */
object Brotli:
  val compressor: Compressor     = BrotliCompressor
  val decompressor: Decompressor = BrotliDecompressor

  def encode(bytes: Chunk[Byte]): Chunk[Byte] =
    Chunk.fromArray(encodeArray(bytes.toArray))

  def decode(bytes: Chunk[Byte]): Chunk[Byte] =
    Chunk.fromArray(decodeArray(bytes.toArray))

  def decodeArray(data: Array[Byte]): Array[Byte] =
    Decoder.decode(data)

  def encodeArray(data: Array[Byte]): Array[Byte] =
    val w = BitWriter()
    w.writeBits(1, 0)
    if data.isEmpty then
      w.writeBits(1, 1)
      w.writeBits(1, 1)
    else
      writeCompressed(w, data)
      w.writeBits(1, 1)
      w.writeBits(1, 1)
    w.finish()
  end encodeArray
end Brotli

private object BrotliCompressor extends Compressor:
  def encoding: ContentEncoding = ContentEncoding.Brotli

  def compress(bytes: Chunk[Byte]): Chunk[Byte] = Brotli.encode(bytes)

  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap {
      zio.ZIO.succeed {
        val w = BitWriter()
        w.writeBits(1, 0)
        in.rechunk(1 << 16).mapChunks { c =>
          if c.isEmpty then Chunk.empty
          else
            writeCompressed(w, c.toArray)
            Chunk.fromArray(w.takeFullBytes())
        } ++ ZStream.fromZIO {
          zio.ZIO.succeed {
            w.writeBits(1, 1)
            w.writeBits(1, 1)
            Chunk.fromArray(w.finish())
          }
        }.flattenChunks
      }
    }
end BrotliCompressor

private object BrotliDecompressor extends Decompressor:
  def encoding: ContentEncoding = ContentEncoding.Brotli

  def decompress(bytes: Chunk[Byte]): Chunk[Byte] = Brotli.decode(bytes)

private def writeCompressed(w: BitWriter, data: Array[Byte]): Unit =
  val cmds = Lz77.compress(data)
  writeHeader(w, data.length)
  val litFreq  = Array.fill(256)(0)
  val cmdFreq  = Array.fill(704)(0)
  val distFreq = Array.fill(Distance.Alphabet)(0)
  val prepared = cmds.map { c =>
    val enc  = Command.encode(c.insert, c.copy, c.dist != 0)
    val dist = if c.dist != 0 then Distance.encode(c.dist) else Distance.Encoded(0, 0, 0)
    cmdFreq(enc.id) += 1
    if c.dist != 0 then distFreq(dist.symbol) += 1
    var i = 0
    while i < c.insert do
      litFreq(c.lits(i) & 0xff) += 1
      i += 1
    (enc, dist, c)
  }
  val litLen  = Huffman.lengths(litFreq, 15)
  val cmdLen  = Huffman.lengths(cmdFreq, 15)
  val distLen = Huffman.lengths(distFreq, 15)
  Huffman.writePrefixCode(w, litLen, 8)
  Huffman.writePrefixCode(w, cmdLen, 10)
  Huffman.writePrefixCode(w, distLen, Distance.AlphabetBits)
  val litCode                                                 = Huffman.codes(litLen)
  val cmdCode                                                 = Huffman.codes(cmdLen)
  val distCode                                                = Huffman.codes(distLen)
  var produced                                                = 0
  def emit(len: Array[Int], code: Array[Int], sym: Int): Unit =
    val n = len(sym)
    if n > 0 then w.writePrefix(n, code(sym))
  prepared.foreach { (enc, dist, c) =>
    emit(cmdLen, cmdCode, enc.id)
    if enc.insert.extraBits > 0 then w.writeBits(enc.insert.extraBits, enc.insert.extra.toLong)
    if enc.copy.extraBits > 0 then w.writeBits(enc.copy.extraBits, enc.copy.extra.toLong)
    var i = 0
    while i < c.lits.length do
      emit(litLen, litCode, c.lits(i) & 0xff)
      i += 1
    produced += c.insert
    if c.dist != 0 && produced < data.length then
      emit(distLen, distCode, dist.symbol)
      if dist.extraBits > 0 then w.writeBits(dist.extraBits, dist.extra.toLong)
      produced += c.copy
  }
end writeCompressed

private def writeHeader(w: BitWriter, len: Int): Unit =
  w.writeBits(1, 0)
  val v = len - 1
  if v < (1 << 16) then
    w.writeBits(2, 0)
    w.writeBits(16, v.toLong)
  else if v < (1 << 20) then
    w.writeBits(2, 1)
    w.writeBits(20, v.toLong)
  else
    w.writeBits(2, 2)
    w.writeBits(24, v.toLong)
  w.writeBits(1, 0)
  w.writeBits(1, 0)
  w.writeBits(1, 0)
  w.writeBits(1, 0)
  w.writeBits(2, 0)
  w.writeBits(4, 0)
  w.writeBits(2, 0)
  w.writeBits(1, 0)
  w.writeBits(1, 0)
end writeHeader
