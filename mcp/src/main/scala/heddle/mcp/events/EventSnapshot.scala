package heddle.mcp.events

import heddle.mcp.protocol.DeliveryHeaders
import zio.json.*
import zio.json.ast.Json
import zio.Chunk

import java.time.Instant
import java.util.Base64
import scala.util.Try

/** Why a subscription snapshot could not be read, written, or applied. Text is a fixed reason or an exception class
  * name: never a path, a secret, a header value, or a file body.
  */
enum StoreError:
  case Unreadable(kind: String)
  case Undecodable
  case Unwritable(kind: String)
  case Unavailable
  case Capacity(limit: Int)
  case Restore(reason: String)

/** One webhook subscription as stored. `lastDeliveryAt` is present only after a delivery succeeded. Header values and
  * the secret are omitted from `toString`.
  */
final case class StoredSubscription private (
    id: String,
    principal: String,
    url: String,
    name: String,
    arguments: Json.Obj,
    refreshBefore: Instant,
    secret: Chunk[Byte],
    headers: DeliveryHeaders,
    lastDeliveryAt: Option[Instant],
):
  override def toString: String =
    s"StoredSubscription($id,$principal,$url,$name,$refreshBefore,$lastDeliveryAt)"

object StoredSubscription:
  private[events] def trusted(
      id: String,
      principal: String,
      url: String,
      name: String,
      arguments: Json.Obj,
      refreshBefore: Instant,
      secret: Chunk[Byte],
      headers: DeliveryHeaders,
      lastDeliveryAt: Option[Instant],
  ): StoredSubscription =
    StoredSubscription(id, principal, url, name, arguments, refreshBefore, secret, headers, lastDeliveryAt)

/** The high-water sequence of one event ring. Bodies are not stored. The next sequence is `head + 1`. */
final case class RingHead(name: String, head: Long)

/** Subscriptions plus ring heads. A missing `lastDeliveryAt` means no successful delivery has been stored. */
final case class EventSnapshot(subscriptions: Vector[StoredSubscription], heads: Vector[RingHead])

object EventSnapshot:
  val empty: EventSnapshot = EventSnapshot(Vector.empty, Vector.empty)

  def encode(snapshot: EventSnapshot): String =
    val subscriptions = Json.Arr(Chunk.fromIterable(snapshot.subscriptions.map(encodeSub)))
    val heads         = Json.Arr(Chunk.fromIterable(snapshot.heads.collect {
      case head if head.name.nonEmpty && head.head > 0L =>
        Json.Obj("name" -> Json.Str(head.name), "head" -> Json.Num(BigDecimal(head.head)))
    }))
    Json.Obj("subscriptions" -> subscriptions, "heads" -> heads).toJson

  def decode(raw: String): Either[StoreError, EventSnapshot] =
    raw.fromJson[Json] match
      case Left(_)              => Left(StoreError.Undecodable)
      case Right(obj: Json.Obj) =>
        (elements(obj, "subscriptions"), elements(obj, "heads")) match
          case (Some(subs), Some(heads)) =>
            Right(EventSnapshot(collect(subs, subscription), collect(heads, ringHead)))
          case _ => Left(StoreError.Undecodable)
      case Right(_) => Left(StoreError.Undecodable)

  /** Drop expired rows, invalid heads, and duplicate ids. Over capacity, evict or fail. */
  def prepare(
      snapshot: EventSnapshot,
      now: Instant,
      capacity: Int,
      evict: Boolean,
  ): Either[StoreError, EventSnapshot] =
    val live  = merge(snapshot.subscriptions).filter(_.refreshBefore.isAfter(now))
    val heads = cleanHeads(snapshot.heads)
    if live.length <= capacity then Right(EventSnapshot(live, heads))
    else if evict then Right(EventSnapshot(prefer(live, capacity), heads))
    else Left(StoreError.Capacity(capacity))

  private def encodeSub(sub: StoredSubscription): Json.Obj =
    val base = Chunk(
      "id"            -> Json.Str(sub.id),
      "principal"     -> Json.Str(sub.principal),
      "url"           -> Json.Str(sub.url),
      "name"          -> Json.Str(sub.name),
      "arguments"     -> sub.arguments,
      "refreshBefore" -> Json.Str(sub.refreshBefore.toString),
      "secret"        -> Json.Str(secretText(sub.secret)),
    )
    val delivered = sub.lastDeliveryAt.fold(Chunk.empty[(String, Json)]) { at =>
      Chunk("lastDeliveryAt" -> Json.Str(at.toString))
    }
    val headers =
      if sub.headers.isEmpty then Chunk.empty[(String, Json)]
      else Chunk("headers" -> Json.Obj(sub.headers.entries.map((name, value) => name -> Json.Str(value))))
    Json.Obj(base ++ delivered ++ headers)

  private def secretText(secret: Chunk[Byte]): String =
    "whsec_" + Base64.getEncoder.encodeToString(secret.toArray)

  private def elements(obj: Json.Obj, key: String): Option[Chunk[Json]] =
    obj.get(key) match
      case None                => Some(Chunk.empty)
      case Some(Json.Arr(els)) => Some(els)
      case Some(_)             => None

  private def collect[A](els: Chunk[Json], read: Json => Option[A]): Vector[A] =
    els.foldLeft(Vector.empty[A]) { (acc, json) =>
      read(json) match
        case Some(value) => acc :+ value
        case None        => acc
    }

  private def subscription(json: Json): Option[StoredSubscription] =
    json match
      case obj: Json.Obj =>
        for
          id        <- text(obj, "id").filter(_.nonEmpty)
          principal <- text(obj, "principal").filter(_.nonEmpty)
          url       <- text(obj, "url").flatMap(raw => WebhookUrl.from(raw).toOption)
          name      <- text(obj, "name").filter(_.nonEmpty)
          arguments <- obj.get("arguments") match
            case Some(args: Json.Obj) => Some(args)
            case _                    => None
          refresh <- text(obj, "refreshBefore").flatMap(raw => Try(Instant.parse(raw)).toOption)
          secret  <- text(obj, "secret").flatMap(raw => WebhookSecret.from(raw).toOption)
          headers <- headersOf(obj)
          last    <- lastOf(obj)
        yield StoredSubscription.trusted(
          id,
          principal,
          url.value,
          name,
          arguments,
          refresh,
          secret.bytes,
          headers,
          last,
        )
      case _ => None

  private def headersOf(obj: Json.Obj): Option[DeliveryHeaders] =
    obj.get("headers") match
      case None       => Some(DeliveryHeaders.empty)
      case Some(json) => DeliveryHeaders.parse(json).toOption

  private def lastOf(obj: Json.Obj): Option[Option[Instant]] =
    obj.get("lastDeliveryAt") match
      case None | Some(Json.Null) => Some(None)
      case Some(Json.Str(raw))    => Try(Instant.parse(raw)).toOption.map(at => Some(at))
      case Some(_)                => None

  private def ringHead(json: Json): Option[RingHead] =
    json match
      case obj: Json.Obj =>
        for
          name <- text(obj, "name").filter(_.nonEmpty)
          head <- obj.get("head") match
            case Some(Json.Num(number)) => Try(number.longValueExact).toOption.filter(_ >= 0L)
            case _                      => None
        yield RingHead(name, head)
      case _ => None

  private def text(obj: Json.Obj, key: String): Option[String] =
    obj.get(key) match
      case Some(Json.Str(value)) => Some(value)
      case _                     => None

  private def merge(subs: Vector[StoredSubscription]): Vector[StoredSubscription] =
    subs.foldLeft(Vector.empty[StoredSubscription]) { (acc, sub) =>
      acc.find(_.id == sub.id) match
        case None           => acc :+ sub
        case Some(existing) =>
          val chosen = if sub.refreshBefore.isBefore(existing.refreshBefore) then existing else sub
          acc.map(item => if item.id == sub.id then chosen else item)
    }

  private def prefer(subs: Vector[StoredSubscription], capacity: Int): Vector[StoredSubscription] =
    subs
      .sortWith { (left, right) =>
        val byTime = left.refreshBefore.compareTo(right.refreshBefore)
        if byTime != 0 then byTime > 0 else left.id < right.id
      }
      .take(capacity)

  private def cleanHeads(heads: Vector[RingHead]): Vector[RingHead] =
    val kept = heads.filter(head => head.name.nonEmpty && head.head >= 0L)
    kept
      .groupBy(_.name)
      .toVector
      .map { (name, group) =>
        RingHead(name, group.foldLeft(0L)((acc, head) => math.max(acc, head.head)))
      }
      .sortBy(_.name)
end EventSnapshot
