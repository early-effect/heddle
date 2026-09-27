package heddle.server

import heddle.BytesLength
import heddle.error.HttpError
import heddle.http.ContentEncoding
import zio.Chunk

trait Decompressor:
  def encoding: ContentEncoding

  /** Inflates at most `limit` bytes. More than that is `BodyTooLarge`; corrupt input is `Malformed`. */
  def decompress(bytes: Chunk[Byte], limit: BytesLength): Either[HttpError, Chunk[Byte]]

object Decompressor:
  val gzip: Decompressor = GzipDecompressor

private object GzipDecompressor extends Decompressor:
  def encoding: ContentEncoding = ContentEncoding.Gzip

  def decompress(bytes: Chunk[Byte], limit: BytesLength): Either[HttpError, Chunk[Byte]] =
    Compressor.gunzip(bytes, limit)
