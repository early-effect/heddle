package heddle.endpoint

import java.nio.charset.StandardCharsets
import zio.Chunk

trait JsonCodec[A]:
  def encode(a: A): String
  def decode(json: String): Either[String, A]

  def encodeBytes(a: A): Chunk[Byte] =
    Chunk.fromArray(encode(a).getBytes(StandardCharsets.UTF_8))

  def decodeBytes(bytes: Chunk[Byte]): Either[String, A] =
    decode(String(bytes.toArray, StandardCharsets.UTF_8))

object JsonCodec:
  def apply[A](using codec: JsonCodec[A]): JsonCodec[A] = codec

  def from[A](encode0: A => String, decode0: String => Either[String, A]): JsonCodec[A] =
    new JsonCodec[A]:
      def encode(a: A): String                    = encode0(a)
      def decode(json: String): Either[String, A] = decode0(json)
