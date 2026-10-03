package heddle.endpoint

import java.nio.charset.StandardCharsets
import heddle.http.{Body, Response, Status}
import scala.collection.immutable.ListMap
import zio.{Chunk, IO, ZIO}

/** How an endpoint's typed error travels: the status each value answers with, and one [[BodyCodec]] for the body.
  *
  * The codec is whatever the caller gave. JSON is only the default, via `BodyCodec`'s given for types that have
  * `Schema` and `JsonCodec`.
  */
final class ErrorCodec[E] private (
    val statusOf: E => Status,
    codec: Option[BodyCodec[E]],
    val docs: List[StatusDoc],
):
  val statuses: Set[Status] = docs.map(_.status).toSet

  private val codes: Set[Int] = statuses.map(_.code)

  def handles(status: Status): Boolean = codes.contains(status.code)

  def encode(e: E): Response =
    codec match
      case None    => Response.empty(statusOf(e))
      case Some(c) => BodyCodec.response(statusOf(e), c, e)

  /** The error as a JSON document, when its body codec is a JSON document. Hosts that are not JSON (MCP structured
    * content) use this and skip the value when it is `None`.
    */
  def json(e: E): Option[String] =
    codec.filter(_.mediaType.isJson).flatMap { c =>
      c.encode(e) match
        case Payload.Strict(bytes)  => Some(BodyCodec.utf8(bytes))
        case Payload.Streamed(_, _) => None
    }

  /** `None` when `status` is not one of this codec's statuses. A body that fails to decode, or whose case answers with
    * another status, fails with [[BodyError]].
    */
  def decode(status: Status, body: Body): IO[BodyError, Option[E]] =
    if !handles(status) then ZIO.succeed(None)
    else read(body).flatMap(checked(status, _))

  /** Reads an error from a JSON document alone, for a host with no status (an MCP `isError` result). `None` when this
    * error is not a JSON document.
    */
  def decodeDocument(raw: String): IO[BodyError, Option[E]] =
    codec.filter(_.mediaType.isJson) match
      case None    => ZIO.succeed(None)
      case Some(c) =>
        val body = Body.fromBytes(Chunk.fromArray(raw.getBytes(StandardCharsets.UTF_8)), Some(c.mediaType))
        c.decode(body).map(Some(_))

  private def read(body: Body): IO[BodyError, Option[E]] =
    codec match
      case None    => ZIO.succeed(None)
      case Some(c) => c.decode(body).map(Some(_))

  private def checked(status: Status, decoded: Option[E]): IO[BodyError, Option[E]] =
    decoded match
      case Some(e) =>
        val expected = statusOf(e)
        if expected.code == status.code then ZIO.succeed(Some(e))
        else ZIO.fail(BodyError.WrongStatus(expected, status))
      case None => ZIO.succeed(None)
end ErrorCodec

object ErrorCodec:
  val none: ErrorCodec[Nothing] =
    ErrorCodec[Nothing](n => n, None, Nil)

  def single[E: BodyCodec](status: Status): ErrorCodec[E] =
    val codec = summon[BodyCodec[E]]
    ErrorCodec(_ => status, Some(codec), List(doc(status, codec.schema, codec.mediaType)))

  /** `statuses(i)` is the status of the case with `ordinal == i`. Built by `Endpoint.outErrors`. */
  private[heddle] def cases[E](statuses: List[Status], ordinal: E => Int)(using
      s: Schema[E],
      codec: BodyCodec[E],
  ): ErrorCodec[E] =
    val table  = statuses.toVector
    val byCode = statuses.zipWithIndex.foldLeft(ListMap.empty[Int, (Status, List[Int])]) { case (acc, (st, i)) =>
      acc.updated(st.code, acc.get(st.code).fold((st, List(i)))((first, is) => (first, is :+ i)))
    }
    val docs = byCode.values.toList.map { (status, ordinals) =>
      val variants = ordinals.map(i => s.cases.lift(i).getOrElse(s.doc)).distinct
      val schema   = variants match
        case one :: Nil => one
        case many       => SchemaDoc.OneOf(None, many)
      doc(status, schema, codec.mediaType)
    }
    // Endpoint.outErrors gives every case a status, so a miss here is a heddle bug and answers 500.
    ErrorCodec(e => table.lift(ordinal(e)).getOrElse(Status.InternalServerError), Some(codec), docs)
  end cases

  private def doc(status: Status, schema: SchemaDoc, mediaType: heddle.http.MediaType): StatusDoc =
    StatusDoc(status, Some(schema), Some(mediaType), status.text)
end ErrorCodec
