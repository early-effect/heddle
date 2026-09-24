package heddle.endpoint

import java.nio.charset.StandardCharsets
import heddle.http.{Body, MediaType, Response, Status}
import zio.Chunk
import zio.json.JsonCodec

/** How an endpoint's typed error travels: the status each value answers with, and one `JsonCodec[E]` for the body. */
final class ErrorCodec[E] private (
    val statusOf: E => Status,
    codec: Option[JsonCodec[E]],
    val docs: List[StatusDoc],
):
  val statuses: Set[Status] = docs.map(_.status).toSet

  private val codes: Set[Int] = statuses.map(_.code)

  def handles(status: Status): Boolean = codes.contains(status.code)

  def encode(e: E): Response =
    codec match
      case None    => Response.empty(statusOf(e))
      case Some(c) =>
        val bytes = Chunk.fromArray(c.encoder.encodeJson(e).toString.getBytes(StandardCharsets.UTF_8))
        Response(statusOf(e)).withBody(Body.fromBytes(bytes, Some(MediaType.Json)))

  def json(e: E): String =
    codec.fold(e.toString)(_.encoder.encodeJson(e).toString)

  /** `None` when `status` is not one of this codec's statuses. A body whose case answers with another status is a
    * `Left`.
    */
  def decode(status: Status, raw: Chunk[Byte]): Option[Either[String, E]] =
    codec.filter(_ => handles(status)).map { c =>
      c.decoder.decodeJson(String(raw.toArray, StandardCharsets.UTF_8)).flatMap { e =>
        val expected = statusOf(e)
        if expected.code == status.code then Right(e)
        else Left(s"error body answers with ${expected.code}, response was ${status.code}")
      }
    }
end ErrorCodec

object ErrorCodec:
  val none: ErrorCodec[Nothing] =
    ErrorCodec[Nothing](n => n, None, Nil)

  def single[E](status: Status)(using s: Schema[E], j: JsonCodec[E]): ErrorCodec[E] =
    ErrorCodec(_ => status, Some(j), List(jsonDoc(status, s.doc)))

  /** `statuses(i)` is the status of the case with `ordinal == i`. Built by `Endpoint.outErrors`. */
  private[heddle] def cases[E](statuses: List[Status], ordinal: E => Int)(using
      s: Schema[E],
      j: JsonCodec[E],
  ): ErrorCodec[E] =
    val table   = statuses.toArray
    val grouped = statuses.zipWithIndex.groupBy(_._1.code)
    val docs    = statuses.map(_.code).distinct.map { code =>
      val ordinals = grouped(code).map(_._2)
      val variants = ordinals.map(i => s.cases.lift(i).getOrElse(s.doc)).distinct
      val schema   = variants match
        case one :: Nil => one
        case many       => SchemaDoc.OneOf(None, many)
      jsonDoc(table(ordinals.head), schema)
    }
    ErrorCodec(e => table(ordinal(e)), Some(j), docs)
  end cases

  private def jsonDoc(status: Status, schema: SchemaDoc): StatusDoc =
    StatusDoc(status, Some(schema), Some(MediaType.Json), status.text)
end ErrorCodec
