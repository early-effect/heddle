package heddle.http.header

import heddle.endpoint.SchemaDoc
import heddle.error.ParamError
import heddle.http.MediaType
import zio.Chunk

/** Parse-on-read typed header. The instance owns the name. Decode failure is `Left`; a missing header is `Missing`. */
trait TypedHeader[A]:
  def name: HeaderName
  def decode(values: Chunk[String]): Either[ParamError, A]
  def encode(value: A): Chunk[String]
  def schema: SchemaDoc

object TypedHeader:
  def apply[A](using t: TypedHeader[A]): TypedHeader[A] = t

  /** One wire line. Absent is `Missing`, and a repeated header is `Malformed`. */
  def single[A](
      headerName: HeaderName,
      schemaDoc: SchemaDoc = SchemaDoc.Str(None),
  )(
      parse: String => Either[ParamError, A],
      render: A => String,
  ): TypedHeader[A] =
    new TypedHeader[A]:
      def name: HeaderName                                     = headerName
      def schema: SchemaDoc                                    = schemaDoc
      def decode(values: Chunk[String]): Either[ParamError, A] =
        values.headOption match
          case None                            => Left(ParamError.Missing)
          case Some(raw) if values.length == 1 => parse(raw)
          case Some(_)                         => Left(ParamError.Malformed(values.mkString(", "), "one value"))
      def encode(value: A): Chunk[String] = Chunk(render(value))

  /** Every line of the name. The parser decides what an empty chunk means. */
  def several[A](headerName: HeaderName, schemaDoc: SchemaDoc)(
      parseAll: Chunk[String] => Either[ParamError, A],
      render: A => Chunk[String],
  ): TypedHeader[A] =
    new TypedHeader[A]:
      def name: HeaderName                                     = headerName
      def schema: SchemaDoc                                    = schemaDoc
      def decode(values: Chunk[String]): Either[ParamError, A] = parseAll(values)
      def encode(value: A): Chunk[String]                      = render(value)

  given TypedHeader[MediaType] =
    single(HeaderName.ContentType)(
      raw => MediaType.parse(raw).toRight(ParamError.Malformed(raw, "a media type")),
      _.render,
    )
end TypedHeader
