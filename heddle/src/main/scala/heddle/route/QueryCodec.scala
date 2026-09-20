package heddle.route

import heddle.endpoint.SchemaDoc
import zio.Chunk

trait QueryCodec[A]:
  def decode(values: Chunk[String]): Either[String, A]
  def encode(value: A): Chunk[String]
  def schema: SchemaDoc
  def required: Boolean

object QueryCodec:
  def apply[A](using c: QueryCodec[A]): QueryCodec[A] = c

  given QueryCodec[String] with
    def decode(values: Chunk[String]): Either[String, String] =
      values.headOption.toRight("missing query parameter")
    def encode(value: String): Chunk[String] = Chunk(value)
    def schema: SchemaDoc                    = SchemaDoc.Str(None)
    def required: Boolean                    = true

  given QueryCodec[Int] with
    def decode(values: Chunk[String]): Either[String, Int] =
      values.headOption.toRight("missing query parameter").flatMap { s =>
        s.toIntOption.toRight(s"invalid int: $s")
      }
    def encode(value: Int): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                 = SchemaDoc.Integer(Some("int32"))
    def required: Boolean                 = true

  given QueryCodec[Long] with
    def decode(values: Chunk[String]): Either[String, Long] =
      values.headOption.toRight("missing query parameter").flatMap { s =>
        s.toLongOption.toRight(s"invalid long: $s")
      }
    def encode(value: Long): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                  = SchemaDoc.Integer(Some("int64"))
    def required: Boolean                  = true

  given QueryCodec[Boolean] with
    def decode(values: Chunk[String]): Either[String, Boolean] =
      values.headOption.toRight("missing query parameter").flatMap {
        case "true" | "1"  => Right(true)
        case "false" | "0" => Right(false)
        case other         => Left(s"invalid boolean: $other")
      }
    def encode(value: Boolean): Chunk[String] = Chunk(if value then "true" else "false")
    def schema: SchemaDoc                     = SchemaDoc.Boolean
    def required: Boolean                     = true
  end given

  given [A](using inner: QueryCodec[A]): QueryCodec[Option[A]] with
    def decode(values: Chunk[String]): Either[String, Option[A]] =
      if values.isEmpty then Right(None) else inner.decode(values).map(Some(_))
    def encode(value: Option[A]): Chunk[String] =
      value.fold(Chunk.empty[String])(inner.encode)
    def schema: SchemaDoc = inner.schema
    def required: Boolean = false
end QueryCodec
