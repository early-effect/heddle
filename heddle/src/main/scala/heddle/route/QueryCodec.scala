package heddle.route

import heddle.endpoint.SchemaDoc
import heddle.error.ParamError
import zio.Chunk

/** Reads a query parameter's values into an `A`, and writes an `A` back as values. */
trait QueryCodec[A]:
  def decode(values: Chunk[String]): Either[ParamError, A]
  def encode(value: A): Chunk[String]
  def schema: SchemaDoc
  def required: Boolean

object QueryCodec:
  def apply[A](using c: QueryCodec[A]): QueryCodec[A] = c

  private def first(values: Chunk[String]): Either[ParamError, String] =
    values.headOption.toRight(ParamError.Missing)

  given QueryCodec[String] with
    def decode(values: Chunk[String]): Either[ParamError, String] =
      first(values)
    def encode(value: String): Chunk[String] = Chunk(value)
    def schema: SchemaDoc                    = SchemaDoc.Str(None)
    def required: Boolean                    = true

  given QueryCodec[Int] with
    def decode(values: Chunk[String]): Either[ParamError, Int] =
      first(values).flatMap(s => s.toIntOption.toRight(ParamError.Malformed(s, "an int")))
    def encode(value: Int): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                 = SchemaDoc.Integer(Some("int32"))
    def required: Boolean                 = true

  given QueryCodec[Long] with
    def decode(values: Chunk[String]): Either[ParamError, Long] =
      first(values).flatMap(s => s.toLongOption.toRight(ParamError.Malformed(s, "a long")))
    def encode(value: Long): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                  = SchemaDoc.Integer(Some("int64"))
    def required: Boolean                  = true

  given QueryCodec[Boolean] with
    def decode(values: Chunk[String]): Either[ParamError, Boolean] =
      first(values).flatMap {
        case "true" | "1"  => Right(true)
        case "false" | "0" => Right(false)
        case other         => Left(ParamError.Malformed(other, "true, false, 1, or 0"))
      }
    def encode(value: Boolean): Chunk[String] = Chunk(if value then "true" else "false")
    def schema: SchemaDoc                     = SchemaDoc.Boolean
    def required: Boolean                     = true
  end given

  given [A](using inner: QueryCodec[A]): QueryCodec[Option[A]] with
    def decode(values: Chunk[String]): Either[ParamError, Option[A]] =
      if values.isEmpty then Right(None) else inner.decode(values).map(Some(_))
    def encode(value: Option[A]): Chunk[String] =
      value.fold(Chunk.empty[String])(inner.encode)
    def schema: SchemaDoc = inner.schema
    def required: Boolean = false
end QueryCodec
