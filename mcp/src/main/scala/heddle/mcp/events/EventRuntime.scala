package heddle.mcp.events

import heddle.mcp.protocol.{ClientRequest, EventName, RpcError}
import zio.json.ast.Json
import zio.{Chunk, Clock, Random, Ref, Semaphore, UIO, URIO, ZIO, ZLayer}

import java.time.Instant

/** In-memory ring and webhook subscriptions. The clock and the sender are read on each call. The permit covers claim
  * and settle only; the POST runs outside it, and `busy` stops a second drain from sending the same message.
  */
private[heddle] object EventRuntime:
  def layer(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
  ): ZLayer[Any, Nothing, Events] =
    ZLayer.fromZIO {
      for
        state <- Ref.make(EventLog.LogState.empty)
        gate  <- Semaphore.make(1)
      yield Live(decls, policy, verification, allow, state, gate)
    }

  private final class Live(
      decls: Chunk[ErasedEvent[?, ?]],
      policy: TtlPolicy,
      verification: VerificationPolicy,
      allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
      state: Ref[EventLog.LogState],
      gate: Semaphore,
  ) extends Events:
    def list(cursor: Option[String]): UIO[Either[RpcError, Json.Obj]] =
      ZIO.succeed(Right(EventLog.list(decls, cursor)))

    def poll(who: Option[Principal], request: ClientRequest.PollEvents): UIO[Either[RpcError, Json.Obj]] =
      now.flatMap { at =>
        exclusiveM { current =>
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
        step  <- exclusiveM { current =>
          val (kept, pre) = EventLog.prelude(current, decls, request, who, at)
          pre match
            case EventLog.Prelude.Stop(error)           => ZIO.succeed((kept, EventLog.SubscribeStep.Failed(error)))
            case EventLog.Prelude.Ask(event, principal) =>
              allow(principal, event.name, request.arguments).map { ok =>
                EventLog.granted(kept, policy, verification, event, principal, request, ok, nonce, at)
              }
        }
        decided <- step match
          case EventLog.SubscribeStep.Failed(error)                   => ZIO.left(error)
          case EventLog.SubscribeStep.Ready(id, truncated, refreshed) =>
            ZIO.right((id, truncated, refreshed))
          case EventLog.SubscribeStep.Challenge(pending) =>
            handshake(pending)
        out <- decided match
          case Left(error)                       => ZIO.left(error)
          case Right((id, truncated, refreshed)) =>
            drain(id, EventLog.Batch) *> exclusive { current =>
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
        exclusiveM { current =>
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
        }
      }

    def publish(
        name: EventName,
        eventId: EventId,
        data: Json,
        at: Instant,
    ): ZIO[WebhookTransport, EmitError, Unit] =
      for
        clockNow <- now
        stored   <- exclusive { current =>
          EventLog.append(current, decls, name, eventId, data, at, clockNow) match
            case Left(error) => (current, Left(error))
            case Right(next) => (next, Right(()))
        }
        _ <- ZIO.fromEither(stored)
        _ <- drainAll
      yield ()

    def maintain: URIO[WebhookTransport, Unit] =
      now.flatMap { at =>
        exclusive { current => (EventLog.housekeep(current, decls, at), ()) }
      } *> drainAll

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
              exclusive { current =>
                EventLog.commit(current, policy, pending, later) match
                  case Left(error)              => (current, Left(error))
                  case Right((next, id, fresh)) => (next, Right((id, pending.truncated, fresh)))
              }
            }
      yield saved

    private def drainAll: URIO[WebhookTransport, Unit] =
      exclusive { current => (current, current.subs.map(_.id)) }.flatMap { ids =>
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
      exclusiveM { current =>
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
          exclusive { current => (EventLog.clearBusy(current, subId, sending.webhookId), ()) } *> ZIO.failCause(cause),
        result =>
          now.flatMap { at =>
            exclusive { current =>
              val outcome = result.fold(EventLog.outcomeOf, EventLog.outcomeOf)
              (EventLog.settle(current, policy, subId, sending.webhookId, outcome, at), ())
            }
          },
      )

    private def post(call: WebhookCall): URIO[WebhookTransport, Either[DeliveryFault, DeliveryReply]] =
      ZIO.serviceWithZIO[WebhookTransport](_.post(call).either)

    private def exclusive[A](f: EventLog.LogState => (EventLog.LogState, A)): UIO[A] =
      gate.withPermit(state.modify(current => f(current).swap))

    private def exclusiveM[A](f: EventLog.LogState => UIO[(EventLog.LogState, A)]): UIO[A] =
      gate.withPermit {
        state.get.flatMap(f).flatMap { (next, value) =>
          state.set(next).as(value)
        }
      }

    private def now: UIO[Instant] =
      Clock.instant

    private def token: UIO[String] =
      Random.nextBytes(16).map(Digests.hex)
  end Live
end EventRuntime
