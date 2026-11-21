package heddle.mcp.events

import heddle.mcp.protocol.{CallbackFailure, ClientRequest, EventName, RpcError, TtlRequest}
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.{Chunk, Duration}

import java.time.Instant
import scala.util.Try

/** The ring and the webhook subscriptions. Pure so the service can hold the lock only around these transitions. One
  * subscription has one in-flight message: a second drain sees `busy` and leaves it alone.
  */
private[events] object EventLog:
  val Batch: Int      = 50
  val NextPollMs: Int = 30000

  final case class Occurred(seq: Long, eventId: String, at: Instant, data: Json)

  final case class Series(head: Long, events: Vector[Occurred])

  enum FlightKind:
    case Occurrence(seq: Long)
    case Gap
    case Terminated

  final case class Flight(
      webhookId: String,
      body: String,
      kind: FlightKind,
      attempts: Int,
      started: Instant,
      next: Instant,
      busy: Boolean,
  )

  final case class Sub(
      id: String,
      principal: String,
      url: String,
      host: String,
      secret: Chunk[Byte],
      previous: Option[(Chunk[Byte], Instant)],
      name: String,
      arguments: Json.Obj,
      argsKey: String,
      until: Instant,
      watermark: Long,
      gap: Boolean,
      active: Boolean,
      failures: Int,
      lastDeliveryAt: Option[Instant],
      lastError: Option[String],
      failedSince: Option[Instant],
      inflight: Option[Flight],
      ephemeral: Boolean,
  )

  final case class Pending(
      principal: String,
      url: String,
      host: String,
      secret: Chunk[Byte],
      name: String,
      arguments: Json.Obj,
      argsKey: String,
      until: Instant,
      watermark: Long,
      truncated: Boolean,
      ephemeral: Boolean,
      nonce: String,
  )

  final case class ChallengeWindow(count: Int, opened: Instant)

  final case class LogState(
      series: Map[String, Series],
      subs: Vector[Sub],
      verified: Set[String],
      challenges: Map[String, ChallengeWindow],
  )

  object LogState:
    val empty: LogState = LogState(Map.empty, Vector.empty, Set.empty, Map.empty)

  enum Prelude:
    case Stop(error: RpcError)
    case Ask(event: ErasedEvent[?, ?], principal: Principal)

  enum SubscribeStep:
    case Failed(error: RpcError)
    case Ready(id: String, truncated: Boolean, refreshed: Boolean)
    case Challenge(pending: Pending)

  final case class Sending(webhookId: String, call: WebhookCall)

  enum Outcome:
    case Ack(reply: DeliveryReply)
    case GiveUp(status: Int)
    case Again(reason: String)
    case Drop

  def housekeep(state: LogState, decls: Chunk[ErasedEvent[?, ?]], now: Instant): LogState =
    val live = state.subs.filter(_.until.isAfter(now)).map(dropGrace(_, now))
    decls.foldLeft(state.copy(subs = live)) { (acc, event) =>
      trimOne(acc, event, now)
    }

  def prelude(
      state: LogState,
      decls: Chunk[ErasedEvent[?, ?]],
      request: ClientRequest.SubscribeEvents,
      who: Option[Principal],
      now: Instant,
  ): (LogState, Prelude) =
    val kept = housekeep(state, decls, now)
    lookup(decls, request.name, request.arguments) match
      case Left(error)  => (kept, Prelude.Stop(error))
      case Right(event) =>
        who match
          case None            => (kept, Prelude.Stop(RpcError.Forbidden(None)))
          case Some(principal) => (kept, Prelude.Ask(event, principal))

  def granted(
      state: LogState,
      policy: TtlPolicy,
      event: ErasedEvent[?, ?],
      principal: Principal,
      request: ClientRequest.SubscribeEvents,
      allowed: Boolean,
      nonce: String,
      now: Instant,
  ): (LogState, SubscribeStep) =
    if !allowed then (state, SubscribeStep.Failed(RpcError.Forbidden(None)))
    else
      checked(event, request) match
        case Left(error)          => (state, SubscribeStep.Failed(error))
        case Right((url, secret)) =>
          val argsKey = CanonicalJson(request.arguments)
          val until   = now.plusMillis(grantMillis(request.ttl, policy))
          findKey(state, principal.value, url.value, event.name.value, argsKey) match
            case Some(existing) =>
              val next = refresh(state, existing, secret.bytes, until, now, policy)
              (next, SubscribeStep.Ready(existing.id, truncated = existing.gap, refreshed = true))
            case None if state.subs.length >= policy.maxSubscriptions =>
              (state, SubscribeStep.Failed(RpcError.ResourceExhausted("subscriptions", Some(policy.maxSubscriptions))))
            case None =>
              val located = position(seriesOf(state, event.name.value), request.cursor, request.maxAgeMs, now)
              val pending = Pending(
                principal.value,
                url.value,
                url.host,
                secret.bytes,
                event.name.value,
                request.arguments,
                argsKey,
                until,
                located.watermark,
                located.truncated,
                ephemeral = event.replay == Replay.Ephemeral,
                nonce,
              )
              if state.verified.contains(verifiedKey(principal.value, url.value)) then
                val (inserted, id) = insert(state, pending)
                (inserted, SubscribeStep.Ready(id, pending.truncated, refreshed = false))
              else
                admitChallenge(state, url.host, policy, now) match
                  case Left(error) => (state, SubscribeStep.Failed(error))
                  case Right(next) => (next, SubscribeStep.Challenge(pending))

  def commit(
      state: LogState,
      policy: TtlPolicy,
      pending: Pending,
      now: Instant,
  ): Either[RpcError, (LogState, String, Boolean)] =
    findKey(state, pending.principal, pending.url, pending.name, pending.argsKey) match
      case Some(existing) =>
        val until = pending.until
        val next  = refresh(state, existing, pending.secret, until, now, policy).copy(
          verified = state.verified + verifiedKey(pending.principal, pending.url)
        )
        Right((next, existing.id, true))
      case None if state.subs.length >= policy.maxSubscriptions =>
        Left(RpcError.ResourceExhausted("subscriptions", Some(policy.maxSubscriptions)))
      case None =>
        val (inserted, id) = insert(state, pending)
        Right((inserted.copy(verified = inserted.verified + verifiedKey(pending.principal, pending.url)), id, false))

  def unsubscribe(
      state: LogState,
      decls: Chunk[ErasedEvent[?, ?]],
      principal: Principal,
      request: ClientRequest.UnsubscribeEvents,
      now: Instant,
  ): (LogState, Either[RpcError, Json.Obj]) =
    val kept = housekeep(state, decls, now)
    lookup(decls, request.name, request.arguments) match
      case Left(error) => (kept, Left(error))
      case Right(_)    =>
        WebhookUrl.from(request.url) match
          case Left(error) => (kept, Left(RpcError.InvalidParams(urlMessage(error))))
          case Right(url)  =>
            val argsKey = CanonicalJson(request.arguments)
            findKey(kept, principal.value, url.value, request.name.value, argsKey) match
              case None      => (kept, Left(RpcError.NotFound(Some("subscription"))))
              case Some(sub) =>
                (kept.copy(subs = kept.subs.filterNot(_.id == sub.id)), Right(Json.Obj()))

  def poll(
      state: LogState,
      decls: Chunk[ErasedEvent[?, ?]],
      event: ErasedEvent[?, ?],
      request: ClientRequest.PollEvents,
      now: Instant,
  ): (LogState, Either[RpcError, Json.Obj]) =
    val kept = housekeep(state, decls, now)
    if !event.supports(Delivery.Poll) then (kept, Left(unsupported("poll")))
    else
      val body =
        if event.replay == Replay.Ephemeral then polled(Chunk.empty, None, truncated = false, hasMore = false)
        else
          val series = seriesOf(kept, event.name.value)
          request.cursor match
            case None =>
              polled(Chunk.empty, Some(series.head.toString), truncated = false, hasMore = false)
            case Some(_) =>
              val start = position(series, request.cursor, request.maxAgeMs, now)
              val limit = request.maxEvents.filter(_ > 0).getOrElse(Batch)
              val scan  = takeMatches(
                series.events.filter(_.seq > start.watermark),
                event,
                request.arguments,
                start.watermark,
                limit,
              )
              polled(scan.events, Some(scan.scanned.toString), start.truncated, scan.hasMore)
      (kept, Right(body))

  def list(decls: Chunk[ErasedEvent[?, ?]], cursor: Option[String]): Json.Obj =
    cursor match
      case Some(_) => Json.Obj("events" -> Json.Arr())
      case None    => Json.Obj("events" -> Json.Arr(decls.map(described)))

  def append(
      state: LogState,
      decls: Chunk[ErasedEvent[?, ?]],
      name: EventName,
      eventId: EventId,
      data: Json,
      at: Instant,
      now: Instant,
  ): Either[EmitError, LogState] =
    decls.find(_.name == name) match
      case None        => Left(EmitError.Unknown(name))
      case Some(event) =>
        if !event.payload(data) then Left(EmitError.Undecodable(name))
        else
          val kept   = housekeep(state, decls, now)
          val series = seriesOf(kept, name.value)
          val seq    = series.head + 1
          val next   = Series(seq, series.events :+ Occurred(seq, eventId.value, at, data))
          Right(trimOne(kept.copy(series = kept.series.updated(name.value, next)), event, now))

  def claim(
      state: LogState,
      policy: TtlPolicy,
      decls: Chunk[ErasedEvent[?, ?]],
      subId: String,
      token: String,
      now: Instant,
      allowed: Boolean,
  ): (LogState, Option[Sending]) =
    state.subs.find(_.id == subId) match
      case None                     => (state, None)
      case Some(sub) if !sub.active => (state, None)
      case Some(sub)                =>
        sub.inflight match
          case Some(flight) if flight.busy                     => (state, None)
          case Some(flight) if flight.next.isAfter(now)        => (state, None)
          case Some(flight) if abandonDue(flight, policy, now) =>
            flight.kind match
              case FlightKind.Terminated => (state.copy(subs = state.subs.filterNot(_.id == subId)), None)
              case _                     =>
                claim(
                  applySub(state, failFlight(sub, flight, now, policy, sub.lastError, count = true)),
                  policy,
                  decls,
                  subId,
                  token,
                  now,
                  allowed,
                )
          case Some(flight) if !allowed && flight.kind != FlightKind.Terminated =>
            val replaced = terminate(sub, token, now)
            (applySub(state, replaced), sendOf(replaced, now))
          case Some(flight) =>
            val retried = flight.copy(attempts = flight.attempts + 1, busy = true)
            val next    = sub.copy(inflight = Some(retried))
            (applySub(state, next), sendOf(next, now))
          case None if !allowed =>
            val replaced = terminate(sub, token, now)
            (applySub(state, replaced), sendOf(replaced, now))
          case None if sub.gap && !sub.ephemeral =>
            val opened = openFlight(sub, gapFlight(sub, token, now), now)
            (applySub(state, opened), sendOf(opened, now))
          case None =>
            decls.find(_.name.value == sub.name) match
              case None        => (state, None)
              case Some(event) =>
                val series          = seriesOf(state, sub.name)
                val rest            = series.events.filter(_.seq > sub.watermark)
                val (skipped, tail) = spanMismatch(rest, event, sub.arguments)
                val advanced        = skipped.lastOption.map(_.seq).getOrElse(sub.watermark)
                val moved           = sub.copy(watermark = advanced)
                tail.headOption match
                  case None      => (applySub(state, moved), None)
                  case Some(hit) =>
                    val opened = openFlight(moved, occurrenceFlight(moved, hit, token, now), now)
                    (applySub(state, opened), sendOf(opened, now))

  def settle(
      state: LogState,
      policy: TtlPolicy,
      subId: String,
      webhookId: String,
      outcome: Outcome,
      now: Instant,
  ): LogState =
    state.subs.find(_.id == subId) match
      case None      => state
      case Some(sub) =>
        sub.inflight match
          case Some(flight) if flight.webhookId == webhookId && flight.kind == FlightKind.Terminated =>
            state.copy(subs = state.subs.filterNot(_.id == subId))
          case Some(flight) if flight.webhookId == webhookId =>
            outcome match
              case Outcome.Ack(_) =>
                flight.kind match
                  case FlightKind.Gap =>
                    applySub(
                      state,
                      sub.copy(
                        inflight = None,
                        gap = false,
                        failures = 0,
                        lastError = None,
                        failedSince = None,
                        lastDeliveryAt = Some(now),
                      ),
                    )
                  case FlightKind.Occurrence(seq) =>
                    applySub(
                      state,
                      sub.copy(
                        inflight = None,
                        watermark = seq,
                        failures = 0,
                        lastError = None,
                        failedSince = None,
                        lastDeliveryAt = Some(now),
                      ),
                    )
                  case FlightKind.Terminated => state
              case Outcome.GiveUp(status) =>
                applySub(state, failFlight(sub, flight, now, policy, Some(httpCategory(status)), count = false))
              case Outcome.Again(reason) =>
                applySub(state, retry(sub, flight, now, policy, reason))
              case Outcome.Drop =>
                applySub(state, failFlight(sub, flight, now, policy, None, count = true))
          case _ => state

  def clearBusy(state: LogState, subId: String, webhookId: String): LogState =
    state.subs.find(_.id == subId) match
      case Some(sub) =>
        sub.inflight match
          case Some(flight) if flight.webhookId == webhookId && flight.busy =>
            applySub(state, sub.copy(inflight = Some(flight.copy(busy = false))))
          case _ => state
      case None => state

  def outcomeOf(fault: DeliveryFault): Outcome =
    fault match
      case DeliveryFault.ConnectionRefused => Outcome.Again(CallbackFailure.ConnectionRefused.wire)
      case DeliveryFault.TimedOut          => Outcome.Again(CallbackFailure.Timeout.wire)
      case DeliveryFault.Tls               => Outcome.Again(CallbackFailure.TlsError.wire)
      case DeliveryFault.Rejected          => Outcome.Drop
      case DeliveryFault.Unavailable       => Outcome.Drop

  def outcomeOf(reply: DeliveryReply): Outcome =
    reply.status match
      case status if status >= 200 && status < 300 => Outcome.Ack(reply)
      case 410 | 413                               => Outcome.GiveUp(reply.status)
      case status                                  => Outcome.Again(httpCategory(status))

  def challengeCall(pending: Pending, now: Instant): WebhookCall =
    val body = Json.Obj("type" -> Json.Str("verification"), "challenge" -> Json.Str(pending.nonce)).toJson
    val id   = s"msg_verification_${pending.nonce}"
    signed(pending.url, id, body, Chunk(pending.secret), Some(subscriptionId(pending)), now)

  def challengeResult(reply: DeliveryReply, nonce: String): Either[RpcError, Unit] =
    if reply.status < 200 || reply.status >= 300 then Left(RpcError.CallbackEndpointError(statusReason(reply.status)))
    else if Digests.sameBytes(Digests.utf8(reply.body.trim), Digests.utf8(nonce)) then Right(())
    else Left(RpcError.CallbackEndpointError(CallbackFailure.ChallengeFailed))

  def challengeFault(fault: DeliveryFault): RpcError =
    fault match
      case DeliveryFault.Rejected          => RpcError.InvalidParams("callback address is not globally routable")
      case DeliveryFault.ConnectionRefused => RpcError.CallbackEndpointError(CallbackFailure.ConnectionRefused)
      case DeliveryFault.TimedOut          => RpcError.CallbackEndpointError(CallbackFailure.Timeout)
      case DeliveryFault.Tls               => RpcError.CallbackEndpointError(CallbackFailure.TlsError)
      case DeliveryFault.Unavailable       => RpcError.Internal("webhook transport is unavailable")

  def subscribeResult(sub: Sub, truncated: Boolean, refreshed: Boolean): Json.Obj =
    val cursor = if sub.ephemeral then Json.Null else Json.Str(sub.watermark.toString)
    val base   = Chunk(
      "id"            -> Json.Str(sub.id),
      "refreshBefore" -> Json.Str(sub.until.toString),
      "cursor"        -> cursor,
      "truncated"     -> Json.Bool(truncated),
    )
    val status = if refreshed then Chunk("deliveryStatus" -> deliveryStatus(sub)) else Chunk.empty
    Json.Obj(base ++ status)

  def subOf(state: LogState, id: String): Option[Sub] = state.subs.find(_.id == id)

  private def lookup(
      decls: Chunk[ErasedEvent[?, ?]],
      name: EventName,
      arguments: Json.Obj,
  ): Either[RpcError, ErasedEvent[?, ?]] =
    decls.find(_.name == name) match
      case None        => Left(RpcError.NotFound(Some("event")))
      case Some(event) => event.arguments(arguments).map(_ => event)

  private def checked(
      event: ErasedEvent[?, ?],
      request: ClientRequest.SubscribeEvents,
  ): Either[RpcError, (WebhookUrl, WebhookSecret)] =
    if !event.supports(Delivery.Webhook) then Left(unsupported("webhook"))
    else
      WebhookUrl.from(request.url) match
        case Left(error) => Left(RpcError.InvalidParams(urlMessage(error)))
        case Right(url)  =>
          WebhookSecret.from(request.secret) match
            case Left(_)       => Left(RpcError.InvalidParams(secretMessage))
            case Right(secret) => Right((url, secret))

  private def refresh(
      state: LogState,
      sub: Sub,
      secret: Chunk[Byte],
      until: Instant,
      now: Instant,
      policy: TtlPolicy,
  ): LogState =
    val grace = now.plusMillis(millis(policy.secretGrace))
    val next  = sub.copy(
      secret = secret,
      previous = Some((sub.secret, grace)),
      until = until,
      active = true,
      failures = 0,
      failedSince = None,
    )
    applySub(state, next)

  private def insert(state: LogState, pending: Pending): (LogState, String) =
    val id  = subscriptionId(pending)
    val sub = Sub(
      id,
      pending.principal,
      pending.url,
      pending.host,
      pending.secret,
      None,
      pending.name,
      pending.arguments,
      pending.argsKey,
      pending.until,
      pending.watermark,
      gap = false,
      active = true,
      failures = 0,
      lastDeliveryAt = None,
      lastError = None,
      failedSince = None,
      inflight = None,
      ephemeral = pending.ephemeral,
    )
    (state.copy(subs = state.subs :+ sub), id)

  private def admitChallenge(
      state: LogState,
      host: String,
      policy: TtlPolicy,
      now: Instant,
  ): Either[RpcError, LogState] =
    val window = millis(policy.challengeWindow)
    state.challenges.get(host) match
      case Some(open) if now.isBefore(open.opened.plusMillis(window)) && open.count >= policy.challengeLimit =>
        Left(RpcError.ResourceExhausted("verification", Some(policy.challengeLimit)))
      case Some(open) if now.isBefore(open.opened.plusMillis(window)) =>
        Right(state.copy(challenges = state.challenges.updated(host, open.copy(count = open.count + 1))))
      case _ =>
        Right(state.copy(challenges = state.challenges.updated(host, ChallengeWindow(1, now))))

  private final case class Located(watermark: Long, truncated: Boolean)

  private def position(series: Series, cursor: Option[String], maxAgeMs: Option[Long], now: Instant): Located =
    cursor match
      case None      => Located(series.head, truncated = false)
      case Some(raw) =>
        parseLong(raw) match
          case None    => Located(series.head, truncated = true)
          case Some(n) =>
            val oldest        = series.events.headOption.map(_.seq).getOrElse(series.head + 1)
            val (base, stale) = if n < oldest - 1 then (oldest - 1, true) else (n, false)
            maxAgeMs match
              case None      => Located(base, stale)
              case Some(age) =>
                val floor   = now.minusMillis(age)
                val waiting = series.events.filter(_.seq > base)
                val tooOld  = waiting.takeWhile(_.at.isBefore(floor))
                tooOld.lastOption match
                  case None       => Located(base, stale)
                  case Some(last) =>
                    val rest = waiting.drop(tooOld.length)
                    rest.headOption match
                      case None    => Located(series.head, truncated = true)
                      case Some(_) => Located(last.seq, truncated = true)

  private final case class Scan(events: Chunk[Json.Obj], scanned: Long, hasMore: Boolean)

  private def takeMatches(
      events: Vector[Occurred],
      event: ErasedEvent[?, ?],
      arguments: Json.Obj,
      start: Long,
      limit: Int,
  ): Scan =
    def loop(rest: Vector[Occurred], got: Chunk[Json.Obj], scanned: Long, room: Int): Scan =
      if room == 0 then Scan(got, scanned, rest.exists(hit => event.matches(arguments, hit.data)))
      else
        rest.headOption match
          case None      => Scan(got, scanned, hasMore = false)
          case Some(hit) =>
            if event.matches(arguments, hit.data) then
              loop(rest.drop(1), got :+ occurrence(event, hit), hit.seq, room - 1)
            else loop(rest.drop(1), got, hit.seq, room)
    loop(events, Chunk.empty, start, limit)

  private def occurrence(event: ErasedEvent[?, ?], hit: Occurred): Json.Obj =
    Json.Obj(
      "eventId"   -> Json.Str(hit.eventId),
      "name"      -> Json.Str(event.name.value),
      "timestamp" -> Json.Str(hit.at.toString),
      "data"      -> hit.data,
    )

  private def polled(events: Chunk[Json.Obj], cursor: Option[String], truncated: Boolean, hasMore: Boolean): Json.Obj =
    Json.Obj(
      "events"     -> Json.Arr(events),
      "cursor"     -> cursor.fold(Json.Null)(Json.Str(_)),
      "truncated"  -> Json.Bool(truncated),
      "hasMore"    -> Json.Bool(hasMore),
      "nextPollMs" -> Json.Num(NextPollMs),
    )

  private def described(event: ErasedEvent[?, ?]): Json.Obj =
    val delivery = Json.Arr(event.modes.map {
      case Delivery.Poll    => Json.Str("poll")
      case Delivery.Webhook => Json.Str("webhook")
    })
    val fields = Chunk(
      "name"          -> Json.Str(event.name.value),
      "inputSchema"   -> event.inputSchema,
      "payloadSchema" -> event.payloadSchema,
      "delivery"      -> delivery,
    ) ++ (if event.description.nonEmpty then Chunk("description" -> Json.Str(event.description)) else Chunk.empty)
    Json.Obj(fields)

  private def trimOne(state: LogState, event: ErasedEvent[?, ?], now: Instant): LogState =
    val name   = event.name.value
    val series = trim(seriesOf(state, name), event.replay, now)
    val oldest = series.events.headOption.map(_.seq)
    val subs   = state.subs.map { sub =>
      if sub.name != name then sub
      else
        val cleared = sub.inflight match
          case Some(flight) =>
            flight.kind match
              case FlightKind.Occurrence(seq) =>
                val gone = oldest match
                  case Some(o) => seq < o
                  case None    => true
                if gone then sub.copy(inflight = None) else sub
              case _ => sub
          case None => sub
        oldest match
          case Some(o) if o > cleared.watermark + 1 =>
            event.replay match
              case Replay.Ephemeral    => cleared.copy(watermark = o - 1, gap = false)
              case Replay.Window(_, _) => cleared.copy(watermark = o - 1, gap = true)
          case None if series.head > cleared.watermark =>
            event.replay match
              case Replay.Ephemeral    => cleared.copy(watermark = series.head, gap = false)
              case Replay.Window(_, _) => cleared.copy(watermark = series.head, gap = true)
          case _ => cleared
    }
    state.copy(series = state.series.updated(name, series), subs = subs)

  private def trim(series: Series, replay: Replay, now: Instant): Series =
    val (maxEvents, maxAge) = replay match
      case Replay.Ephemeral        => (Replay.DefaultEvents, Replay.DefaultAge)
      case Replay.Window(max, age) => (max, age)
    val floor = now.minusMillis(millis(maxAge))
    val fresh = series.events.filterNot(_.at.isBefore(floor))
    val kept  = if fresh.length <= maxEvents then fresh else fresh.takeRight(maxEvents)
    series.copy(events = kept)

  private def spanMismatch(
      events: Vector[Occurred],
      event: ErasedEvent[?, ?],
      arguments: Json.Obj,
  ): (Vector[Occurred], Vector[Occurred]) =
    val skipped = events.takeWhile(hit => !event.matches(arguments, hit.data))
    (skipped, events.drop(skipped.length))

  private def openFlight(sub: Sub, flight: Flight, now: Instant): Sub =
    sub.copy(inflight = Some(flight.copy(started = now, next = now, attempts = 1, busy = true)))

  private def occurrenceFlight(sub: Sub, hit: Occurred, token: String, now: Instant): Flight =
    val cursor = if sub.ephemeral then Json.Null else Json.Str(sub.watermark.toString)
    val body   = Json
      .Obj(
        "eventId"   -> Json.Str(hit.eventId),
        "name"      -> Json.Str(sub.name),
        "timestamp" -> Json.Str(hit.at.toString),
        "data"      -> hit.data,
        "cursor"    -> cursor,
      )
      .toJson
    Flight(s"msg_$token", body, FlightKind.Occurrence(hit.seq), 1, now, now, busy = true)

  private def gapFlight(sub: Sub, token: String, now: Instant): Flight =
    val cursor = if sub.ephemeral then Json.Null else Json.Str(sub.watermark.toString)
    val body   = Json.Obj("type" -> Json.Str("gap"), "cursor" -> cursor).toJson
    Flight(s"msg_gap_$token", body, FlightKind.Gap, 1, now, now, busy = true)

  private def terminate(sub: Sub, token: String, now: Instant): Sub =
    val body   = Json.Obj("type" -> Json.Str("terminated"), "error" -> RpcError.Forbidden(None).json).toJson
    val flight = Flight(s"msg_terminated_$token", body, FlightKind.Terminated, 1, now, now, busy = true)
    sub.copy(inflight = Some(flight))

  private def sendOf(sub: Sub, now: Instant): Option[Sending] =
    sub.inflight.filter(_.busy).map { flight =>
      val secrets =
        Chunk(sub.secret) ++ Chunk.fromIterable(sub.previous.filter((_, until) => until.isAfter(now)).map(_._1))
      Sending(flight.webhookId, signed(sub.url, flight.webhookId, flight.body, secrets, Some(sub.id), now))
    }

  private def signed(
      url: String,
      webhookId: String,
      body: String,
      secrets: Chunk[Chunk[Byte]],
      subId: Option[String],
      now: Instant,
  ): WebhookCall =
    val stamp     = now.getEpochSecond
    val signature = WebhookSigner.header(webhookId, stamp, body, secrets)
    val headers   = Chunk(
      "webhook-id"        -> webhookId,
      "webhook-timestamp" -> stamp.toString,
      "webhook-signature" -> signature,
      "content-type"      -> "application/json",
    ) ++ Chunk.fromIterable(subId.map(id => "X-MCP-Subscription-Id" -> id))
    WebhookCall(url, headers, body)

  private def retry(sub: Sub, flight: Flight, now: Instant, policy: TtlPolicy, reason: String): Sub =
    if abandonDue(flight, policy, now) then failFlight(sub, flight, now, policy, Some(reason), count = true)
    else
      sub.copy(
        inflight = Some(flight.copy(busy = false, next = now.plusMillis(backoffMillis(flight.attempts)))),
        lastError = Some(reason),
        failedSince = sub.failedSince.orElse(Some(now)),
      )

  private def failFlight(
      sub: Sub,
      flight: Flight,
      now: Instant,
      policy: TtlPolicy,
      category: Option[String],
      count: Boolean,
  ): Sub =
    flight.kind match
      case FlightKind.Terminated => sub.copy(inflight = None, active = false)
      case FlightKind.Gap        =>
        counted(sub.copy(inflight = None, gap = false, lastError = category.orElse(sub.lastError)), now, policy, count)
      case FlightKind.Occurrence(seq) =>
        counted(
          sub.copy(inflight = None, watermark = seq, lastError = category.orElse(sub.lastError)),
          now,
          policy,
          count,
        )

  private def counted(sub: Sub, now: Instant, policy: TtlPolicy, count: Boolean): Sub =
    if !count then sub
    else
      val failures = sub.failures + 1
      sub.copy(
        failures = failures,
        failedSince = sub.failedSince.orElse(Some(now)),
        active = sub.active && failures < policy.suspendAfter,
      )

  private def abandonDue(flight: Flight, policy: TtlPolicy, now: Instant): Boolean =
    flight.attempts >= policy.maxAttempts || !now.isBefore(flight.started.plusMillis(millis(policy.retryWindow)))

  private def backoffMillis(attempts: Int): Long =
    val shift = math.min(math.max(attempts - 1, 0), 4)
    1000L << shift

  private def applySub(state: LogState, sub: Sub): LogState =
    state.copy(subs = state.subs.map(current => if current.id == sub.id then sub else current))

  private def dropGrace(sub: Sub, now: Instant): Sub =
    sub.copy(previous = sub.previous.filter((_, until) => until.isAfter(now)))

  private def deliveryStatus(sub: Sub): Json.Obj =
    Json.Obj(
      Chunk("active" -> Json.Bool(sub.active)) ++
        Chunk.fromIterable(sub.lastDeliveryAt.map(at => "lastDeliveryAt" -> Json.Str(at.toString))) ++
        Chunk.fromIterable(sub.lastError.map(reason => "lastError" -> Json.Str(reason))) ++
        Chunk.fromIterable(sub.failedSince.map(at => "failedSince" -> Json.Str(at.toString)))
    )

  private def seriesOf(state: LogState, name: String): Series =
    state.series.getOrElse(name, Series(0L, Vector.empty))

  private def findKey(state: LogState, principal: String, url: String, name: String, argsKey: String): Option[Sub] =
    state.subs.find(sub => sub.principal == principal && sub.url == url && sub.name == name && sub.argsKey == argsKey)

  private def subscriptionId(pending: Pending): String =
    idOf(pending.principal, pending.url, pending.name, pending.argsKey)

  private def idOf(principal: String, url: String, name: String, argsKey: String): String =
    val digest = Digests.sha256(Digests.utf8(s"$principal\n$url\n$name\n$argsKey"))
    "sub_" + Digests.hex(digest.take(16))

  private def verifiedKey(principal: String, url: String): String = s"$principal\n$url"

  private def grantMillis(ttl: TtlRequest, policy: TtlPolicy): Long =
    val floor = millis(policy.floor)
    val cap   = millis(policy.cap)
    val raw   = ttl match
      case TtlRequest.Absent     => millis(policy.default)
      case TtlRequest.Unbounded  => cap
      case TtlRequest.Millis(ms) => ms
    math.max(floor, math.min(cap, raw))

  private def millis(duration: Duration): Long =
    Try(duration.toMillis).toOption.filter(_ > 0).getOrElse(1L)

  private def parseLong(raw: String): Option[Long] =
    if raw.isEmpty || raw.exists(c => c < '0' || c > '9') then None
    else raw.toLongOption

  private def httpCategory(status: Int): String =
    if status >= 500 then CallbackFailure.Http5xx.wire else CallbackFailure.Http4xx.wire

  private def statusReason(status: Int): CallbackFailure =
    if status >= 500 then CallbackFailure.Http5xx else CallbackFailure.Http4xx

  private def unsupported(mode: String): RpcError =
    RpcError.Unsupported(Json.Obj("feature" -> Json.Str("deliveryMode"), "value" -> Json.Str(mode)))

  private val secretMessage = "callback secret must be whsec_ and standard base64 of 24 to 64 bytes"

  private def urlMessage(error: WebhookUrlError): String =
    error match
      case WebhookUrlError.NotRoutable                        => "callback address is not globally routable"
      case WebhookUrlError.NotHttps                           => "callback url must be https"
      case WebhookUrlError.UserInfo                           => "callback url must not contain userinfo"
      case WebhookUrlError.Malformed | WebhookUrlError.NoHost => "callback url is not valid"
end EventLog
