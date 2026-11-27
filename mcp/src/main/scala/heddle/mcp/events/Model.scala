package heddle.mcp.events

import heddle.mcp.protocol.EventName
import zio.{Chunk, Duration}

import java.net.URI
import java.util.Base64
import scala.quoted.*
import scala.util.Try

/** A delivery mode this server implements. Push is not a member, so a declaration cannot advertise it. */
enum Delivery:
  case Poll
  case Webhook

/** Where cursors come from. `Ephemeral` keeps a short log for live webhooks and puts `null` on the wire, so poll
  * returns nothing. A window is the shared ring poll and webhook both read.
  */
enum Replay:
  case Ephemeral
  case Window(maxEvents: Int, maxAge: Duration)

object Replay:
  val DefaultEvents: Int   = 1000
  val DefaultAge: Duration = Duration.fromMillis(10L * 60L * 1000L)
  val default: Replay      = Window(DefaultEvents, DefaultAge)

  def accepted(replay: Replay): Boolean =
    replay match
      case Ephemeral        => true
      case Window(max, age) => max >= 1 && age.toMillis > 0
end Replay

/** Who a webhook subscription belongs to. Blank is not a principal. */
opaque type Principal = String

object Principal:
  def apply(value: String): Principal = value

  extension (p: Principal) def value: String = p
end Principal

/** The author's id for one occurrence. Empty, over-long, and control characters cannot be built. */
opaque type EventId = String

object EventId:
  def from(raw: String): Either[EventIdError, EventId] =
    if raw.isEmpty then Left(EventIdError.Empty)
    else if raw.length > 256 then Left(EventIdError.TooLong(raw.length))
    else
      raw.indexWhere(c => c < ' ' || c == 0x7f) match
        case -1 => Right(raw)
        case at => Left(EventIdError.Control(at))

  inline def apply(inline raw: String): EventId = ${ literal('raw) }

  extension (id: EventId) def value: String = id

  private def literal(raw: Expr[String])(using Quotes): Expr[EventId] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("EventId(...) takes a literal; use EventId.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not an event id: ${e.message}")
      case Some((s, Right(_))) => Expr(s)
end EventId

enum EventIdError(val message: String):
  case Empty                extends EventIdError("an event id is empty")
  case TooLong(length: Int) extends EventIdError(s"an event id is at most 256 characters, and this is $length")
  case Control(at: Int)     extends EventIdError(s"an event id has a control character at $at")

/** An `https` callback with no userinfo. An IP literal must be globally routable. A hostname is checked at delivery. */
opaque type WebhookUrl = String

object WebhookUrl:
  def from(raw: String): Either[WebhookUrlError, WebhookUrl] =
    Try(URI(raw)).toOption match
      case None      => Left(WebhookUrlError.Malformed)
      case Some(uri) =>
        if uri.getScheme != "https" then Left(WebhookUrlError.NotHttps)
        else if Option(uri.getUserInfo).exists(_.nonEmpty) then Left(WebhookUrlError.UserInfo)
        else
          Option(uri.getHost).filter(_.nonEmpty) match
            case None       => Left(WebhookUrlError.NoHost)
            case Some(host) =>
              AddressPolicy.parseLiteral(host) match
                case Some(bytes) if !AddressPolicy.globallyRoutable(bytes) => Left(WebhookUrlError.NotRoutable)
                case _                                                     => Right(raw)

  extension (url: WebhookUrl)
    def value: String = url
    def host: String  = Try(URI(url)).toOption.flatMap(uri => Option(uri.getHost)).filter(_.nonEmpty).getOrElse("")
end WebhookUrl

enum WebhookUrlError:
  case Malformed
  case NotHttps
  case UserInfo
  case NoHost
  case NotRoutable

/** `whsec_` plus standard base64 of 24 to 64 bytes. The server never mints one. */
opaque type WebhookSecret = Chunk[Byte]

object WebhookSecret:
  def from(raw: String): Either[WebhookSecretError, WebhookSecret] =
    if !raw.startsWith("whsec_") then Left(WebhookSecretError.Prefix)
    else
      Try(Base64.getDecoder.decode(raw.drop("whsec_".length))).toOption match
        case None                             => Left(WebhookSecretError.Encoding)
        case Some(bytes) if bytes.length < 24 => Left(WebhookSecretError.Length)
        case Some(bytes) if bytes.length > 64 => Left(WebhookSecretError.Length)
        case Some(bytes)                      => Right(Chunk.fromArray(bytes))

  extension (secret: WebhookSecret) def bytes: Chunk[Byte] = secret
end WebhookSecret

enum WebhookSecretError:
  case Prefix
  case Encoding
  case Length

/** Grants and limits, already clamped by `sanitize`.
  *
  * `floor`, `default`, and `cap` are the server's TTL. A requested `ttlMs` below the floor is raised, an omitted
  * `ttlMs` uses `default`, and a longer one, including `null`, is the cap. A grant ends at its refresh time.
  * `renewHealthy` moves that time only after a delivery succeeds, or after a restore that recorded no failure.
  */
final case class TtlPolicy(
    default: Duration,
    cap: Duration,
    floor: Duration,
    maxSubscriptions: Int,
    maxAttempts: Int,
    suspendAfter: Int,
    secretGrace: Duration,
    retryWindow: Duration,
    challengeLimit: Int,
    challengeWindow: Duration,
)

object TtlPolicy:
  val default: TtlPolicy = sanitize(
    TtlPolicy(
      default = Duration.fromMillis(30L * 60L * 1000L),
      cap = Duration.fromMillis(30L * 60L * 1000L),
      floor = Duration.fromMillis(60L * 1000L),
      maxSubscriptions = 256,
      maxAttempts = 5,
      suspendAfter = 5,
      secretGrace = Duration.fromMillis(5L * 60L * 1000L),
      retryWindow = Duration.fromMillis(15L * 60L * 1000L),
      challengeLimit = 8,
      challengeWindow = Duration.fromMillis(60L * 1000L),
    )
  )

  def sanitize(policy: TtlPolicy): TtlPolicy =
    val cap   = atLeast(policy.cap, 1)
    val floor = clamp(policy.floor, Duration.fromMillis(1), cap)
    val grant = clamp(policy.default, floor, cap)
    TtlPolicy(
      default = grant,
      cap = cap,
      floor = floor,
      maxSubscriptions = math.max(policy.maxSubscriptions, 1),
      maxAttempts = math.max(policy.maxAttempts, 1),
      suspendAfter = math.max(policy.suspendAfter, 1),
      secretGrace = atLeast(policy.secretGrace, 1),
      retryWindow = atLeast(policy.retryWindow, 1),
      challengeLimit = math.max(policy.challengeLimit, 1),
      challengeWindow = atLeast(policy.challengeWindow, 1),
    )

  private def atLeast(duration: Duration, minMillis: Long): Duration =
    Duration.fromMillis(math.max(millis(duration), minMillis))

  private def clamp(duration: Duration, floor: Duration, cap: Duration): Duration =
    val value = millis(duration)
    val low   = millis(floor)
    val high  = millis(cap)
    Duration.fromMillis(math.max(low, math.min(high, value)))

  private def millis(duration: Duration): Long =
    Try(duration.toMillis).toOption.filter(_ > 0).getOrElse(1L)
end TtlPolicy

/** Whether the first subscribe to a `(principal, url)` must pass the nonce challenge. The server chooses this when it
  * builds the event layer, the same way it chooses `TtlPolicy`.
  */
enum VerificationPolicy:
  /** POST a signed `verification` body and commit only when a `2xx` echoes the nonce. */
  case Required

  /** Commit the first subscribe with no challenge POST. A hosted receiver answers with its own body. */
  case Skip
end VerificationPolicy

object VerificationPolicy:
  val default: VerificationPolicy = Required

/** Why `Events.publish` refused. Not an exception: the event never entered the ring. */
enum EmitError:
  case Unknown(name: EventName)
  case Undecodable(name: EventName)

  /** The subscription store refused the snapshot. The event was not appended. */
  case NotDurable
end EmitError
