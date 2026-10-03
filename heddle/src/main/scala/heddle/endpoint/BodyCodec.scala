package heddle.endpoint

import java.nio.charset.StandardCharsets
import heddle.http.{Body, Form, MediaType, Response, Status}
import heddle.http.header.{HeaderName, Headers}
import heddle.sse.{ServerSentEvent, SseCodec}
import zio.json.JsonCodec
import zio.stream.ZStream
import zio.{Chunk, IO, ZIO}

/** What a body codec writes. The endpoint stamps [[BodyCodec.mediaType]] on the way out. */
enum Payload:
  case Strict(bytes: Chunk[Byte])
  case Streamed(bytes: ZStream[Any, Throwable, Byte], length: Option[Long])

/** How a value travels as one body: the media type, the documented schema, and both directions.
  *
  * JSON is the default for any `A` that already has `Schema` and `JsonCodec` (see [[BodyCodecLowPriority]]). A given
  * `BodyCodec[A]` the user writes replaces that default. `String` is JSON unless a more specific given says otherwise;
  * plain text is [[Text]].
  */
trait BodyCodec[A]:
  def mediaType: MediaType
  def schema: SchemaDoc

  /** Response headers this body adds when the response does not already carry them. */
  def headers: Headers = Headers.empty

  def encode(value: A): Payload
  def decode(body: Body): IO[BodyError, A]

/** A typed server-sent event body. `Client.call` rejects it; `Client.subscribe` reads it. */
type EventStream = ZStream[Any, Throwable, ServerSentEvent]

object BodyCodec extends BodyCodecLowPriority:
  def json[A](using schema: Schema[A], codec: JsonCodec[A]): BodyCodec[A] =
    val documented = schema.doc
    new BodyCodec[A]:
      def mediaType: MediaType      = MediaType.Json
      def schema: SchemaDoc         = documented
      def encode(value: A): Payload =
        Payload.Strict(Chunk.fromArray(codec.encoder.encodeJson(value).toString.getBytes(StandardCharsets.UTF_8)))
      def decode(body: Body): IO[BodyError, A] =
        bytes(body).flatMap { raw =>
          ZIO.fromEither(
            codec.decoder.decodeJson(utf8(raw)).left.map(BodyError.Json(_))
          )
        }
    end new
  end json

  def text[A](mediaType: MediaType): TextPartiallyApplied[A] =
    TextPartiallyApplied(mediaType)

  def bytes[A](mediaType: MediaType): BytesPartiallyApplied[A] =
    BytesPartiallyApplied(mediaType)

  /** `application/x-www-form-urlencoded`. */
  given BodyCodec[Form] = new BodyCodec[Form]:
    def mediaType: MediaType         = MediaType.FormUrlEncoded
    def schema: SchemaDoc            = SchemaDoc.Str(None)
    def encode(value: Form): Payload =
      Payload.Strict(Chunk.fromArray(value.render.getBytes(StandardCharsets.UTF_8)))
    def decode(body: Body): IO[BodyError, Form] =
      body.asForm.mapError(BodyError.Unreadable(_))

  given BodyCodec[EventStream] = new BodyCodec[EventStream]:
    def mediaType: MediaType                = MediaType.EventStream
    def schema: SchemaDoc                   = SchemaDoc.Str(None)
    override def headers: Headers           = Headers.empty.add(HeaderName.CacheControl, "no-cache")
    def encode(value: EventStream): Payload =
      Payload.Streamed(value.flatMap(event => ZStream.fromChunk(SseCodec.encode(event))), None)
    def decode(body: Body): IO[BodyError, EventStream] =
      ZIO.succeed(SseCodec.stream(body.toStream))

  def response[A](status: Status, codec: BodyCodec[A], value: A): Response =
    val base = Response(status).withBody(payload(codec, value))
    codec.headers.toChunk.foldLeft(base) { (res, header) =>
      if res.headers.has(header.name) then res else res.addHeader(header.name, header.value)
    }

  def payload[A](codec: BodyCodec[A], value: A): Body =
    codec.encode(value) match
      case Payload.Strict(raw)           => Body.fromBytes(raw, Some(codec.mediaType))
      case Payload.Streamed(raw, length) => Body.stream(raw, Some(codec.mediaType), length)

  private[heddle] def bytes(body: Body): IO[BodyError, Chunk[Byte]] =
    body.collect.mapError(BodyError.Unreadable(_))

  private[heddle] def utf8(raw: Chunk[Byte]): String =
    String(raw.toArray, StandardCharsets.UTF_8)

  /** `None` or `utf-8`. Any other charset is [[BodyError.Charset]]. */
  private[endpoint] def charset(mediaType: MediaType): Either[BodyError, Unit] =
    mediaType.charset match
      case None                                         => Right(())
      case Some(name) if name.equalsIgnoreCase("utf-8") => Right(())
      case Some(name)                                   => Left(BodyError.Charset(name))
end BodyCodec

final class TextPartiallyApplied[A](mediaType: MediaType):
  def apply(write: A => String, read: String => A): BodyCodec[A] =
    val mt = mediaType
    new BodyCodec[A]:
      def mediaType: MediaType      = mt
      def schema: SchemaDoc         = SchemaDoc.Str(None)
      def encode(value: A): Payload =
        Payload.Strict(Chunk.fromArray(write(value).getBytes(StandardCharsets.UTF_8)))
      def decode(body: Body): IO[BodyError, A] =
        BodyCodec.charset(mt) match
          case Left(err) => ZIO.fail(err)
          case Right(_)  => BodyCodec.bytes(body).map(raw => read(BodyCodec.utf8(raw)))
  end apply
end TextPartiallyApplied

final class BytesPartiallyApplied[A](mediaType: MediaType):
  def apply(write: A => Chunk[Byte], read: Chunk[Byte] => A): BodyCodec[A] =
    val mt = mediaType
    new BodyCodec[A]:
      def mediaType: MediaType                 = mt
      def schema: SchemaDoc                    = SchemaDoc.Str(Some("binary"))
      def encode(value: A): Payload            = Payload.Strict(write(value))
      def decode(body: Body): IO[BodyError, A] =
        BodyCodec.bytes(body).map(read)

/** Lower priority than a `given BodyCodec[A]` the user wrote, including one in `A`'s companion. */
private trait BodyCodecLowPriority:
  given jsonDocument[A: Schema: JsonCodec]: BodyCodec[A] = BodyCodec.json
