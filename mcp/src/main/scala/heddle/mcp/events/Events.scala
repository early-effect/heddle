package heddle.mcp.events

import heddle.mcp.protocol.{ClientRequest, EventName, RpcError}
import zio.json.*
import zio.json.ast.Json
import zio.{Duration, Scope, UIO, URIO, ZIO}

import java.time.Instant

/** The event ring and its webhook subscriptions. The clock and random source are the runtime's defaults, read when a
  * method runs, so a test moves `TestClock` and the same methods see it. The layer does not provide them.
  */
trait Events:
  def list(cursor: Option[String]): UIO[Either[RpcError, Json.Obj]]
  def poll(who: Option[Principal], request: ClientRequest.PollEvents): UIO[Either[RpcError, Json.Obj]]
  def subscribe(
      who: Option[Principal],
      request: ClientRequest.SubscribeEvents,
  ): URIO[WebhookTransport, Either[RpcError, Json.Obj]]
  def unsubscribe(
      who: Option[Principal],
      request: ClientRequest.UnsubscribeEvents,
  ): UIO[Either[RpcError, Json.Obj]]
  def publish(
      name: EventName,
      eventId: EventId,
      data: Json,
      at: Instant,
  ): ZIO[WebhookTransport, EmitError, Unit]
  def maintain: URIO[WebhookTransport, Unit]

object Events:
  /** What an events route needs besides the author's own environment. The clock stays on the app runtime. */
  type Env = Events & WebhookTransport

  def publish[A: JsonEncoder](
      name: EventName,
      eventId: EventId,
      data: A,
      at: Instant,
  ): ZIO[Env, EmitError, Unit] =
    data.toJsonAST match
      case Left(_)     => ZIO.fail(EmitError.Undecodable(name))
      case Right(json) => ZIO.serviceWithZIO[Events](_.publish(name, eventId, json, at))

  /** Expire, trim, and deliver whatever is due. Tests call this after moving `TestClock`. */
  def maintain: ZIO[Env, Nothing, Unit] =
    ZIO.serviceWithZIO[Events](_.maintain)

  /** Production loop. A test does not install it. */
  def maintained: ZIO[Env & Scope, Nothing, Unit] =
    maintain.repeat(zio.Schedule.spaced(Duration.fromSeconds(1))).forkScoped.unit
end Events
