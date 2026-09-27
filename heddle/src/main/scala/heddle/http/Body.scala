package heddle.http

import java.nio.charset.StandardCharsets
import zio.{Chunk, Task, UIO, ZIO}
import zio.stream.ZStream

enum Body:
  case Empty
  case Bytes(bytes: Chunk[Byte], contentType: Option[MediaType])
  case Stream(stream: ZStream[Any, Throwable, Byte], contentType: Option[MediaType], contentLength: Option[Long])

  def toStream: ZStream[Any, Throwable, Byte] =
    this match
      case Empty                => ZStream.empty
      case Bytes(bytes, _)      => ZStream.fromChunk(bytes)
      case Stream(stream, _, _) => stream

  /** The bytes of an in-memory body. A stream has none until read: use [[collect]] or [[toStream]]. */
  def strict: Option[Chunk[Byte]] =
    this match
      case Empty           => Some(Chunk.empty)
      case Bytes(bytes, _) => Some(bytes)
      case Stream(_, _, _) => None

  /** [[strict]] as UTF-8. */
  def text: Option[String] = strict.map(b => String(b.toArray, StandardCharsets.UTF_8))

  /** Collects any body (including streams) as UTF-8. */
  def utf8: Task[String] =
    collect.map(bytes => String(bytes.toArray, StandardCharsets.UTF_8))

  def collect: Task[Chunk[Byte]] =
    this match
      case Empty           => ZIO.succeed(Chunk.empty)
      case Bytes(bytes, _) => ZIO.succeed(bytes)
      case Stream(s, _, _) => s.runCollect

  def mediaType: Option[MediaType] =
    this match
      case Empty                     => None
      case Bytes(_, contentType)     => contentType
      case Stream(_, contentType, _) => contentType

  def length: Option[Long] =
    this match
      case Empty             => Some(0L)
      case Bytes(bytes, _)   => Some(bytes.length.toLong)
      case Stream(_, _, len) => len

  def isEmpty: Boolean =
    this match
      case Empty           => true
      case Bytes(bytes, _) => bytes.isEmpty
      case Stream(_, _, _) => false

  def asForm: Task[Form] =
    utf8.map(Form.decode)

  private def boundary: Either[MultipartError, String] =
    mediaType.flatMap(_.params.collectFirst { case ("boundary", b) => b }).toRight(MultipartError.Undeclared)

  def asMultipart: ZIO[Any, Throwable | MultipartError, Chunk[FormField]] =
    ZIO.fromEither(boundary).flatMap(b => collect.flatMap(bytes => ZIO.fromEither(Multipart.parse(bytes, b))))

  def asMultipartStream: ZStream[Any, Throwable | MultipartError, FormField] =
    boundary.fold(ZStream.fail(_), Multipart.decode(toStream, _))
end Body

object Body:
  val empty: Body = Empty

  def text(value: String, contentType: MediaType = MediaType.TextUtf8): Body =
    Bytes(Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8)), Some(contentType))

  def json(value: String): Body =
    text(value, MediaType.JsonUtf8)

  def html(value: String): Body =
    text(value, MediaType.HtmlUtf8)

  def fromBytes(bytes: Chunk[Byte], contentType: Option[MediaType] = None): Body =
    if bytes.isEmpty then Empty else Bytes(bytes, contentType)

  def stream(
      stream: ZStream[Any, Throwable, Byte],
      contentType: Option[MediaType] = None,
      length: Option[Long] = None,
  ): Body =
    Stream(stream, contentType, length)

  def form(form: Form): Body =
    text(form.render, MediaType.FormUrlEncoded)

  /** A `multipart/form-data` body under a fresh random boundary. */
  def multipart(fields: Chunk[FormField]): UIO[Body] =
    Multipart.boundary.map(multipart(fields, _))

  def multipart(fields: Chunk[FormField], boundary: String): Body =
    val bytes = Multipart.encode(fields, boundary)
    fromBytes(bytes, Some(MediaType("multipart", "form-data", params = List("boundary" -> boundary))))
end Body
