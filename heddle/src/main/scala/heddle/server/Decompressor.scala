package heddle.server

import heddle.http.ContentEncoding
import zio.Chunk

trait Decompressor:
  def encoding: ContentEncoding
  def decompress(bytes: Chunk[Byte]): Chunk[Byte]

object Decompressor:
  val gzip: Decompressor = GzipDecompressor

private object GzipDecompressor extends Decompressor:
  def encoding: ContentEncoding                   = ContentEncoding.Gzip
  def decompress(bytes: Chunk[Byte]): Chunk[Byte] = Compressor.gunzip(bytes)
