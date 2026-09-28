package heddle.mcp.apps.host

import heddle.mcp.apps.{MetaProblem, Narrowing, UiUri}
import heddle.mcp.apps.ui.LoggingLevel
import heddle.mcp.protocol.ToolName
import java.time.Instant
import zio.*
import zio.json.JsonCodec
import zio.stream.ZStream

/** Which framing of a view a message belongs to. Every mount gets the next one, so a reload is never the same view. */
opaque type Generation = Long

object Generation:
  def apply(n: Long): Generation = n

  given JsonCodec[Generation] = JsonCodec.long

  extension (g: Generation) def value: Long = g

/** What a view, its relay, or the host did. */
enum Action derives JsonCodec:
  case Mount
  case Request(method: String, tool: Option[ToolName], arguments: Option[Digest])
  case Notification(method: String)
  case Navigated
  case Teardown

/** Why the pipeline refused one thing a view asked for. */
enum Denial(val message: String) derives JsonCodec:
  case NotLinked(tool: ToolName) extends Denial(s"${tool.value} is not an app tool linked to this view on this server")
  case ConsentRefused(outcome: ConsentOutcome) extends Denial(s"the user did not allow it ($outcome)")
  case NotOffered(method: String)              extends Denial(s"this host does not offer $method")
  case NotTheView(uri: String)                 extends Denial(s"$uri is not this view's resource")
  case NotALink(url: String)                   extends Denial(s"$url is not an http or https link")
  case Malformed(reason: String)               extends Denial(reason)

/** What the host decided. */
enum Decision derives JsonCodec:
  case Allowed
  case Denied(reason: Denial)
  case ConsentAsked(outcome: ConsentOutcome)
  case Narrowed(narrowing: Narrowing)
  case Dropped(problem: MetaProblem)
  case HashMismatch(pinned: Digest, served: Digest)
  case DroppedAfterNavigate
  case TornDown(reason: String)
  case Logged(level: LoggingLevel)
end Decision

final case class AuditEvent(
    at: Instant,
    server: ServerName,
    view: UiUri,
    generation: Generation,
    action: Action,
    decision: Decision,
) derives JsonCodec

/** Every decision a host made about its views, in order. `history` is the recent log; `events` follows it live. */
final class Audit private (hub: Hub[AuditEvent], log: Ref[Chunk[AuditEvent]], capacity: Int):
  def record(event: AuditEvent): UIO[Unit] =
    log.update(l => (l :+ event).takeRight(capacity)) *> hub.publish(event).unit

  def history: UIO[Chunk[AuditEvent]] = log.get

  def events: ZStream[Any, Nothing, AuditEvent] = ZStream.fromHub(hub)

object Audit:
  /** Keeps the last `capacity` events; the hub closes with the layer. */
  def layer(capacity: Int = 10000): ULayer[Audit] =
    ZLayer.scoped(
      for
        hub <- ZIO.acquireRelease(Hub.unbounded[AuditEvent])(_.shutdown)
        log <- Ref.make(Chunk.empty[AuditEvent])
      yield Audit(hub, log, capacity)
    )
end Audit
