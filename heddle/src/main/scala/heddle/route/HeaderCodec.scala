package heddle.route

import heddle.endpoint.SchemaDoc

trait HeaderCodec[A]:
  def decode(value: Option[String]): Either[String, A]
  def schema: SchemaDoc
  def required: Boolean

object HeaderCodec:
  def apply[A](using c: HeaderCodec[A]): HeaderCodec[A] = c

  given HeaderCodec[String] with
    def decode(value: Option[String]): Either[String, String] =
      value.toRight("missing header")
    def schema: SchemaDoc = SchemaDoc.Str(None)
    def required: Boolean = true

  given [A](using inner: HeaderCodec[A]): HeaderCodec[Option[A]] with
    def decode(value: Option[String]): Either[String, Option[A]] =
      value match
        case None    => Right(None)
        case Some(_) => inner.decode(value).map(Some(_))
    def schema: SchemaDoc = inner.schema
    def required: Boolean = false
end HeaderCodec
