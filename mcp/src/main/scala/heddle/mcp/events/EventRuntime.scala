package heddle.mcp.events

import heddle.mcp.protocol.{ClientRequest, EventName, RpcError}
import zio.json.ast.Json
import zio.{Chunk, Clock, Duration, Exit, IO, Queue, Random, Ref, Scope, Semaphore, UIO, URIO, ZIO, ZLayer}

import java.time.Instant

/** Ring and webhook subscriptions. The clock and the sender are read on each call. The permit covers one transition. A
  * snapshot write finishes before the transition is visible, and a refused write leaves the previous log in place.
  */
private[heddle] object EventRuntime:
  def layer(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
  ): ZLayer[Any, Nothing, Events] =
    ZLayer.scoped {
      for
        store <- SubscriptionStore.openMemory(policy.maxSubscriptions)
        live  <- assemble(decls, policy, verification, allow, store, EventLog.LogState.empty)
      yield live
    }

  def stored(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
  ): ZLayer[SubscriptionStore, StoreError, Events] =
    ZLayer.scoped {
      for
        store   <- ZIO.service[SubscriptionStore]
        loaded  <- store.snapshot
        at      <- Clock.instant
        initial <- ZIO.fromEither(EventLog.restore(decls, loaded, at))
        live    <- assemble(decls, policy, verification, allow, store, initial)
      yield live
    }

  private def assemble(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
      store: SubscriptionStore,
      initial: EventLog.LogState,
  ): ZIO[Scope, Nothing, Events] =
    for
      state   <- Ref.make(initial)
      gate    <- Semaphore.make(1)
      arms    <- Ref.make(Map.empty[String, Scope.Closeable])
      expired <- Queue.unbounded[String]
      live = Live(decls, policy, verification, allow, state, gate, store, arms, expired)
      _ <- ZIO.foreachDiscard(initial.subs)(sub => live.arm(sub.id))
      _ <- live.supervise.forkScoped
      _ <- ZIO.addFinalizer(live.preserveAll)
    yield live

  private final class Live(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
      state: Ref[EventLog.LogState],
      gate: Semaphore,
      store: SubscriptionStore,
      arms: Ref[Map[String, Scope.Closeable]],
      expired: Queue[String],
  ) extends Events:
    def list(cursor: Option[String]): UIO[Either[RpcError, Json.Obj]] =
      ZIO.succeed(Right(EventLog.list(decls, cursor)))

    def poll(who: Option[Principal], request: ClientRequest.PollEvents): UIO[Either[RpcError, Json.Obj]] =
      now.flatMap { at =>
        looseM { current =>
          val kept = EventLog.housekeep(current, decls, at)
          decls.find(_.name == request.name) match
            case None        => ZIO.succeed((kept, Left(RpcError.NotFound(Some("event")))))
            case Some(event) =>
              event.arguments(request.arguments) match
                case Left(error) => ZIO.succeed((kept, Left(error)))
                case Right(_)    =>
                  who match
                    case None            => ZIO.succeed(EventLog.poll(kept, decls, event, request, at))
                    case Some(principal) =>
                      allow(principal, event.name, request.arguments).map { ok =>
                        if !ok then (kept, Left(RpcError.Forbidden(None)))
                        else EventLog.poll(kept, decls, event, request, at)
                      }
        }
      }

    def subscribe(
        who: Option[Principal],
        request: ClientRequest.SubscribeEvents,
    ): URIO[WebhookTransport, Either[RpcError, Json.Obj]] =
      for
        at    <- now
        nonce <- token
        step  <- strictM { current =>
          val (kept, pre) = EventLog.prelude(current, decls, request, who, at)
          pre match
            case EventLog.Prelude.Stop(error)           => ZIO.succeed((kept, EventLog.SubscribeStep.Failed(error)))
            case EventLog.Prelude.Ask(event, principal) =>
              allow(principal, event.name, request.arguments).map { ok =>
                EventLog.granted(kept, policy, verification, event, principal, request, ok, nonce, at)
              }
        }.catchAll(error => ZIO.succeed(EventLog.SubscribeStep.Failed(asRpc(error))))
        decided <- step match
          case EventLog.SubscribeStep.Failed(error)                   => ZIO.left(error)
          case EventLog.SubscribeStep.Ready(id, truncated, refreshed) => ZIO.right((id, truncated, refreshed))
          case EventLog.SubscribeStep.Challenge(pending)              => handshake(pending)
        out <- decided match
          case Left(error)                       => ZIO.left(error)
          case Right((id, truncated, refreshed)) =>
            drain(id, EventLog.Batch) *> loose { current =>
              EventLog.subOf(current, id) match
                case None      => (current, Left(RpcError.NotFound(Some("subscription"))))
                case Some(sub) => (current, Right(EventLog.subscribeResult(sub, truncated, refreshed)))
            }
      yield out

    def unsubscribe(
        who: Option[Principal],
        request: ClientRequest.UnsubscribeEvents,
    ): UIO[Either[RpcError, Json.Obj]] =
      now.flatMap { at =>
        strictM { current =>
          who match
            case None =>
              val kept = EventLog.housekeep(current, decls, at)
              ZIO.succeed((kept, Left(RpcError.Forbidden(None))))
            case Some(principal) =>
              decls.find(_.name == request.name) match
                case None =>
                  val kept = EventLog.housekeep(current, decls, at)
                  ZIO.succeed((kept, Left(RpcError.NotFound(Some("event")))))
                case Some(event) =>
                  allow(principal, event.name, request.arguments).map { ok =>
                    if !ok then
                      val kept = EventLog.housekeep(current, decls, at)
                      (kept, Left(RpcError.Forbidden(None)))
                    else EventLog.unsubscribe(current, decls, principal, request, at)
                  }
        }.catchAll(error => ZIO.succeed(Left(asRpc(error))))
      }

    def publish(
        name: EventName,
        eventId: EventId,
        data: Json,
        at: Instant,
    ): ZIO[WebhookTransport, EmitError, Unit] =
      for
        clockNow <- now
        stored   <- strict { current =>
          EventLog.append(current, decls, name, eventId, data, at, clockNow) match
            case Left(error) => (current, Left(error))
            case Right(next) => (next, Right(()))
        }.mapError(_ => EmitError.NotDurable)
        _ <- ZIO.fromEither(stored)
        _ <- drainAll
      yield ()

    def maintain: URIO[WebhookTransport, Unit] =
      now.flatMap { at =>
        loose { current => (EventLog.housekeep(current, decls, at), ()) }
      } *> drainAll

    def renewHealthy(by: Duration): IO[StoreError, Unit] =
      now.flatMap { at =>
        strict { current => (EventLog.renewHealthy(current, decls, by, at), ()) }
      }

    private[EventRuntime] def arm(id: String): UIO[Unit] =
      for
        child <- Scope.make
        _     <- child.extend(store.watch(id, expired.offer(id).unit))
        _     <- arms.update(_.updated(id, child))
      yield ()

    private[EventRuntime] def supervise: UIO[Unit] =
      expired.take.flatMap(onExpired).forever

    private[EventRuntime] def preserveAll: UIO[Unit] =
      arms.get.flatMap { all =>
        ZIO.foreachDiscard(all.keys) { id =>
          store.markKept(id) *> disarm(id)
        }
      }

    private def handshake(
        pending: EventLog.Pending
    ): URIO[WebhookTransport, Either[RpcError, (String, Boolean, Boolean)]] =
      for
        at     <- now
        posted <- post(EventLog.challengeCall(pending, at))
        verdict = posted match
          case Left(fault)  => Left(EventLog.challengeFault(fault))
          case Right(reply) => EventLog.challengeResult(reply, pending.nonce)
        saved <- verdict match
          case Left(error) => ZIO.left(error)
          case Right(_)    =>
            now.flatMap { later =>
              strict { current =>
                EventLog.commit(current, policy, pending, later) match
                  case Left(error)              => (current, Left(error))
                  case Right((next, id, fresh)) => (next, Right((id, pending.truncated, fresh)))
              }.catchAll(error => ZIO.succeed(Left(asRpc(error))))
            }
      yield saved

    private def drainAll: URIO[WebhookTransport, Unit] =
      loose { current => (current, current.subs.map(_.id)) }.flatMap { ids =>
        ZIO.foreachDiscard(ids)(id => drain(id, EventLog.Batch))
      }

    private def drain(subId: String, budget: Int): URIO[WebhookTransport, Unit] =
      if budget <= 0 then ZIO.unit
      else
        for
          stamp <- token
          at    <- now
          next  <- claimOne(subId, stamp, at)
          _     <- next match
            case None          => ZIO.unit
            case Some(sending) => deliver(subId, sending) *> drain(subId, budget - 1)
        yield ()

    private def claimOne(subId: String, stamp: String, at: Instant): UIO[Option[EventLog.Sending]] =
      looseM { current =>
        current.subs.find(_.id == subId) match
          case None      => ZIO.succeed((current, None))
          case Some(sub) =>
            EventName.from(sub.name) match
              case Left(_) =>
                ZIO.succeed(EventLog.claim(current, policy, decls, subId, stamp, at, allowed = false))
              case Right(name) =>
                allow(Principal(sub.principal), name, sub.arguments).map { ok =>
                  EventLog.claim(current, policy, decls, subId, stamp, at, ok)
                }
      }

    private def deliver(subId: String, sending: EventLog.Sending): URIO[WebhookTransport, Unit] =
      val posted = post(sending.call)
      posted.foldCauseZIO(
        cause =>
          loose { current => (EventLog.clearBusy(current, subId, sending.webhookId), ()) } *> ZIO.failCause(cause),
        result =>
          now.flatMap { at =>
            loose { current =>
              val outcome = result.fold(EventLog.outcomeOf, EventLog.outcomeOf)
              (EventLog.settle(current, policy, subId, sending.webhookId, outcome, at), ())
            }
          },
      )

    private def post(call: WebhookCall): URIO[WebhookTransport, Either[DeliveryFault, DeliveryReply]] =
      ZIO.serviceWithZIO[WebhookTransport](_.post(call).either)

    private def onExpired(id: String): UIO[Unit] =
      now.flatMap { at =>
        loose { current =>
          current.subs.find(_.id == id) match
            case Some(sub) if sub.until.isAfter(at) => (current, true)
            case Some(_)                            => (current.copy(subs = current.subs.filterNot(_.id == id)), false)
            case None                               => (current, false)
        }.flatMap { live =>
          if live then rewatch(id) else disarm(id)
        }
      }

    private def rewatch(id: String): UIO[Unit] =
      arms.get.flatMap { all =>
        all.get(id) match
          case None        => arm(id)
          case Some(child) => child.extend(store.watch(id, expired.offer(id).unit))
      }

    private def disarm(id: String): UIO[Unit] =
      arms.modify { all =>
        val closing = all.get(id) match
          case None        => ZIO.unit
          case Some(child) => child.close(Exit.unit)
        (closing, all - id)
      }.flatten

    private def realign(before: EventLog.LogState, after: EventLog.LogState): UIO[Unit] =
      val beforeIds = before.subs.map(_.id).toSet
      val afterIds  = after.subs.map(_.id).toSet
      for
        _ <- ZIO.foreachDiscard(beforeIds.filterNot(afterIds.contains))(disarm)
        _ <- ZIO.foreachDiscard(after.subs.filterNot(sub => beforeIds.contains(sub.id)))(sub => arm(sub.id))
        _ <- ZIO.foreachDiscard(after.subs.filter(sub => beforeIds.contains(sub.id))) { sub =>
          store.touch(sub.id, sub.until)
        }
      yield ()

    private def strict[A](f: EventLog.LogState => (EventLog.LogState, A)): IO[StoreError, A] =
      gate.withPermit {
        state.get.flatMap { current =>
          val (next, value) = f(current)
          save(current, next).as(value)
        }
      }

    private def strictM[A](f: EventLog.LogState => UIO[(EventLog.LogState, A)]): IO[StoreError, A] =
      gate.withPermit {
        state.get.flatMap { current =>
          f(current).flatMap { (next, value) =>
            save(current, next).as(value)
          }
        }
      }

    private def loose[A](f: EventLog.LogState => (EventLog.LogState, A)): UIO[A] =
      gate.withPermit {
        state.get.flatMap { current =>
          val (next, value) = f(current)
          save(current, next).catchAll(_ => ZIO.unit).as(value)
        }
      }

    private def looseM[A](f: EventLog.LogState => UIO[(EventLog.LogState, A)]): UIO[A] =
      gate.withPermit {
        state.get.flatMap { current =>
          f(current).flatMap { (next, value) =>
            save(current, next).catchAll(_ => ZIO.unit).as(value)
          }
        }
      }

    private def save(current: EventLog.LogState, next: EventLog.LogState): IO[StoreError, Unit] =
      if EventLog.snapshot(current) == EventLog.snapshot(next) then state.set(next)
      else store.replace(EventLog.snapshot(next)) *> realign(current, next) *> state.set(next)

    private def asRpc(error: StoreError): RpcError =
      error match
        case StoreError.Capacity(limit) => RpcError.ResourceExhausted("subscriptions", Some(limit))
        case _                          => RpcError.Internal("subscription store")

    private def now: UIO[Instant] =
      Clock.instant

    private def token: UIO[String] =
      Random.nextBytes(16).map(Digests.hex)
  end Live
end EventRuntime
