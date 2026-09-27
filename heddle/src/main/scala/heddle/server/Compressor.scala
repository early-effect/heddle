package heddle.server

import heddle.BytesLength
import heddle.error.HttpError
import heddle.http.ContentEncoding
import zio.Chunk
import zio.stream.ZStream

trait Compressor:
  def encoding: ContentEncoding
  def compress(bytes: Chunk[Byte]): Chunk[Byte]
  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte]

object Compressor:
  def gzip: Compressor = CompressorLive.gzip

  /** Inflates at most `limit` bytes. More than that is `BodyTooLarge`; corrupt input is `Malformed`. */
  def gunzip(bytes: Chunk[Byte], limit: BytesLength): Either[HttpError, Chunk[Byte]] =
    CompressorLive.gunzip(bytes, limit.toLong)
