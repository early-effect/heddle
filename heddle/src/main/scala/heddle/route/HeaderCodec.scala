package heddle.route

import heddle.endpoint.SchemaDoc
import heddle.error.ParamError

/** Reads a header's value (absent or present) into an `A`, and writes an `A` back. */
trait HeaderCodec[A]:
  def decode(value: Option[String]): Either[ParamError, A]
  def encode(value: A): Option[String]
  def schema: SchemaDoc
  def required: Boolean

object HeaderCodec:
  def apply[A](using c: HeaderCodec[A]): HeaderCodec[A] = c

  given HeaderCodec[String] with
    def decode(value: Option[String]): Either[ParamError, String] =
      value.toRight(ParamError.Missing)
    def encode(value: String): Option[String] = Some(value)
    def schema: SchemaDoc                     = SchemaDoc.Str(None)
    def required: Boolean                     = true

  given [A](using inner: HeaderCodec[A]): HeaderCodec[Option[A]] with
    def decode(value: Option[String]): Either[ParamError, Option[A]] =
      value match
        case None    => Right(None)
        case Some(_) => inner.decode(value).map(Some(_))
    def encode(value: Option[A]): Option[String] =
      value.flatMap(inner.encode)
    def schema: SchemaDoc = inner.schema
    def required: Boolean = false
end HeaderCodec
