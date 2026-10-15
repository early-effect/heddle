package heddle.route

import java.time.Instant
import heddle.endpoint.SchemaDoc
import heddle.error.ParamError
import heddle.http.Url
import heddle.http.header.{EntityTag, HttpDate, TypedHeader}
import zio.Chunk

/** Reads every value of one header name into an `A`, and writes an `A` back as one wire line per element.
  *
  * An empty chunk omits the header. The name is chosen at the call site, so one type can fill several headers
  * (`Instant` is `Date` or `If-Modified-Since`). A type that owns its name is a [[TypedHeader]] and is summoned here by
  * the bridge given.
  */
trait HeaderCodec[A]:
  def decode(values: Chunk[String]): Either[ParamError, A]
  def encode(value: A): Chunk[String]
  def schema: SchemaDoc
  def required: Boolean

object HeaderCodec:
  def apply[A](using c: HeaderCodec[A]): HeaderCodec[A] = c

  private def first(values: Chunk[String]): Either[ParamError, String] =
    values.headOption.toRight(ParamError.Missing)

  given HeaderCodec[String] with
    def decode(values: Chunk[String]): Either[ParamError, String] =
      first(values)
    def encode(value: String): Chunk[String] = Chunk(value)
    def schema: SchemaDoc                    = SchemaDoc.Str(None)
    def required: Boolean                    = true

  given HeaderCodec[Int] with
    def decode(values: Chunk[String]): Either[ParamError, Int] =
      first(values).flatMap(s => s.toIntOption.toRight(ParamError.Malformed(s, "an int")))
    def encode(value: Int): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                 = SchemaDoc.Integer(Some("int32"))
    def required: Boolean                 = true

  given HeaderCodec[Long] with
    def decode(values: Chunk[String]): Either[ParamError, Long] =
      first(values).flatMap(s => s.toLongOption.toRight(ParamError.Malformed(s, "a long")))
    def encode(value: Long): Chunk[String] = Chunk(value.toString)
    def schema: SchemaDoc                  = SchemaDoc.Integer(Some("int64"))
    def required: Boolean                  = true

  given HeaderCodec[Boolean] with
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

  /** An HTTP-date (RFC 9110 §5.6.7). `Date`, `Expires`, `Last-Modified`, and `If-Modified-Since` share it. */
  given HeaderCodec[Instant] with
    def decode(values: Chunk[String]): Either[ParamError, Instant] =
      first(values).flatMap(s => HttpDate.parse(s).toRight(ParamError.Malformed(s, "an HTTP-date")))
    def encode(value: Instant): Chunk[String] = Chunk(HttpDate.render(value))
    def schema: SchemaDoc                     = SchemaDoc.Str(Some("date-time"))
    def required: Boolean                     = true

  /** A header whose value is a URL. `Location` is the usual name. */
  given HeaderCodec[Url] with
    def decode(values: Chunk[String]): Either[ParamError, Url] =
      first(values).flatMap(s => Url.decode(s).left.map(ParamError.Url(_)))
    def encode(value: Url): Chunk[String] = Chunk(value.render)
    def schema: SchemaDoc                 = SchemaDoc.Str(Some("uri"))
    def required: Boolean                 = true

  /** `If-Match` and `If-None-Match`. One line per tag. `*` is dropped, as [[EntityTag.parseList]] drops it. */
  given HeaderCodec[Chunk[EntityTag]] with
    def decode(values: Chunk[String]): Either[ParamError, Chunk[EntityTag]] =
      if values.isEmpty then Left(ParamError.Missing)
      else Right(values.flatMap(EntityTag.parseList))
    def encode(value: Chunk[EntityTag]): Chunk[String] = value.map(_.render)
    def schema: SchemaDoc                              = SchemaDoc.Array(SchemaDoc.Str(None))
    def required: Boolean                              = true

  given [A](using t: TypedHeader[A]): HeaderCodec[A] with
    def decode(values: Chunk[String]): Either[ParamError, A] = t.decode(values)
    def encode(value: A): Chunk[String]                      = t.encode(value)
    def schema: SchemaDoc                                    = t.schema
    def required: Boolean                                    = true

  given [A](using inner: HeaderCodec[A]): HeaderCodec[Option[A]] with
    def decode(values: Chunk[String]): Either[ParamError, Option[A]] =
      if values.isEmpty then Right(None) else inner.decode(values).map(Some(_))
    def encode(value: Option[A]): Chunk[String] =
      value.fold(Chunk.empty[String])(inner.encode)
    def schema: SchemaDoc = inner.schema
    def required: Boolean = false
end HeaderCodec
