package heddle.server

import heddle.http.ContentEncoding
import zio.Chunk
import zio.stream.ZStream

trait Compressor:
  def encoding: ContentEncoding
  def compress(bytes: Chunk[Byte]): Chunk[Byte]
  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte]

object Compressor:
  def gzip: Compressor = CompressorLive.gzip

  def gunzip(bytes: Chunk[Byte]): Chunk[Byte] =
    CompressorLive.gunzip(bytes)
