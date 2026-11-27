package heddle.mcp.events

import zio.{Clock, Duration, IO, Ref, Scope, Semaphore, UIO, ULayer, ZIO, ZLayer}

import java.time.Instant

/** Durable subscriptions and ring heads. The default layer is memory. A file layer is opt-in and JVM-only.
  *
  * Each retained subscription is tied to the current scope and to its `refreshBefore`. The entry is dropped when that
  * scope closes, unless `markKept` ran, and when the refresh time passes. Capacity is the most live rows the store will
  * accept.
  */
trait SubscriptionStore:
  def snapshot: IO[StoreError, EventSnapshot]
  def replace(snapshot: EventSnapshot): IO[StoreError, Unit]
  def retain(entry: StoredSubscription): ZIO[Scope, StoreError, Unit]
  def renew(id: String, refreshBefore: Instant): IO[StoreError, Unit]
  def markKept(id: String): UIO[Unit]
  private[events] def watch(id: String, onExpire: UIO[Unit]): ZIO[Scope, Nothing, Unit]
  private[events] def drop(id: String): IO[StoreError, Unit]
  private[events] def touch(id: String, refreshBefore: Instant): UIO[Unit]

object SubscriptionStore:
  def memory: ULayer[SubscriptionStore] =
    memory(TtlPolicy.default.maxSubscriptions)

  def memory(capacity: Int): ULayer[SubscriptionStore] =
    ZLayer.scoped(openMemory(capacity))

  def file(path: String): ZLayer[Any, StoreError, SubscriptionStore] =
    file(path, TtlPolicy.default.maxSubscriptions)

  def file(path: String, capacity: Int): ZLayer[Any, StoreError, SubscriptionStore] =
    SubscriptionStorePlatform.file(path, math.max(capacity, 1))

  private[events] def openMemory(capacity: Int): ZIO[Scope, Nothing, SubscriptionStore] =
    for
      book <- Book.make(math.max(capacity, 1), Journal.memory, EventSnapshot.empty)
      _    <- ZIO.addFinalizer(book.seal)
    yield book

  private[events] def open(capacity: Int, journal: Journal): ZIO[Scope, StoreError, SubscriptionStore] =
    val cap = math.max(capacity, 1)
    for
      loaded  <- journal.load
      decoded <- loaded match
        case None      => ZIO.succeed(EventSnapshot.empty)
        case Some(raw) => ZIO.fromEither(EventSnapshot.decode(raw))
      at     <- Clock.instant
      pruned <- ZIO.fromEither(EventSnapshot.prepare(decoded, at, cap, evict = true))
      encoded = EventSnapshot.encode(pruned)
      _ <- loaded match
        case Some(raw) if raw != encoded => journal.save(encoded)
        case _                           => ZIO.unit
      book <- Book.make(cap, journal, pruned)
      _    <- ZIO.addFinalizer(book.seal)
    yield book

  private[events] trait Journal:
    def load: IO[StoreError, Option[String]]
    def save(text: String): IO[StoreError, Unit]

  private[events] object Journal:
    val memory: Journal = new Journal:
      def load: IO[StoreError, Option[String]]     = ZIO.none
      def save(text: String): IO[StoreError, Unit] = ZIO.succeed(text).unit

  private[events] final class Row(
      val deadline: Ref[Instant],
      val keep: Ref[Boolean],
      val watched: Ref[Boolean],
  )

  private[events] final class Book(
      capacity: Int,
      journal: Journal,
      snap: Ref[EventSnapshot],
      rows: Ref[Map[String, Row]],
      permit: Semaphore,
      closed: Ref[Boolean],
  ) extends SubscriptionStore:
    def seal: UIO[Unit] = closed.set(true)

    def snapshot: IO[StoreError, EventSnapshot] =
      lock(snap.get)

    def replace(snapshot: EventSnapshot): IO[StoreError, Unit] =
      lock(write(snapshot, evict = false))

    def retain(entry: StoredSubscription): ZIO[Scope, StoreError, Unit] =
      lock {
        for
          at      <- Clock.instant
          current <- snap.get
          live   = current.subscriptions.filter(_.refreshBefore.isAfter(at))
          exists = live.exists(_.id == entry.id)
          _ <-
            if !exists && live.length >= capacity then ZIO.fail(StoreError.Capacity(capacity))
            else write(current.copy(subscriptions = upsert(live, entry), heads = current.heads), evict = false)
        yield ()
      } *> watch(entry.id, drop(entry.id).catchAll(_ => ZIO.unit))

    def renew(id: String, refreshBefore: Instant): IO[StoreError, Unit] =
      lock {
        snap.get.flatMap { current =>
          current.subscriptions.find(_.id == id) match
            case None        => ZIO.unit
            case Some(found) =>
              val updated = StoredSubscription.trusted(
                found.id,
                found.principal,
                found.url,
                found.name,
                found.arguments,
                refreshBefore,
                found.secret,
                found.headers,
                found.lastDeliveryAt,
              )
              val next = current.copy(subscriptions = current.subscriptions.map { item =>
                if item.id == id then updated else item
              })
              write(next, evict = false)
        }
      }

    def markKept(id: String): UIO[Unit] =
      lock {
        rows.get.flatMap { map =>
          map.get(id) match
            case None      => ZIO.unit
            case Some(row) => row.keep.set(true)
        }
      }

    def watch(id: String, onExpire: UIO[Unit]): ZIO[Scope, Nothing, Unit] =
      ZIO.scopeWith { scope =>
        lock(rows.get.map(_.get(id))).flatMap {
          case None      => ZIO.unit
          case Some(row) =>
            row.watched.modify(seen => (seen, true)).flatMap { seen =>
              val arm = if seen then ZIO.unit else scope.addFinalizer(release(id))
              arm *> sleepUntil(row, onExpire).forkScoped.unit
            }
        }
      }

    def drop(id: String): IO[StoreError, Unit] =
      lock {
        snap.get.flatMap { current =>
          if current.subscriptions.exists(_.id == id) then
            write(current.copy(subscriptions = current.subscriptions.filterNot(_.id == id)), evict = false)
          else ZIO.unit
        }
      }

    def touch(id: String, refreshBefore: Instant): UIO[Unit] =
      lock {
        rows.get.flatMap { map =>
          map.get(id) match
            case None      => ZIO.unit
            case Some(row) => row.deadline.set(refreshBefore)
        }
      }

    private def release(id: String): UIO[Unit] =
      lock {
        rows.get.flatMap { map =>
          map.get(id) match
            case None      => ZIO.unit
            case Some(row) =>
              row.keep.get.flatMap { kept =>
                if kept then ZIO.unit else erase(id)
              }
        }
      }.catchAll(_ => ZIO.unit)

    private def erase(id: String): IO[StoreError, Unit] =
      snap.get.flatMap { current =>
        if current.subscriptions.exists(_.id == id) then
          write(current.copy(subscriptions = current.subscriptions.filterNot(_.id == id)), evict = false)
        else ZIO.unit
      }

    private def write(next: EventSnapshot, evict: Boolean): IO[StoreError, Unit] =
      closed.get.flatMap { done =>
        if done then ZIO.unit
        else
          Clock.instant.flatMap { at =>
            EventSnapshot.prepare(next, at, capacity, evict) match
              case Left(error)   => ZIO.fail(error)
              case Right(pruned) =>
                journal.save(EventSnapshot.encode(pruned)) *> snap.set(pruned) *> install(pruned)
          }
      }

    private[events] def install(next: EventSnapshot): UIO[Unit] =
      rows.get.flatMap { current =>
        val wanted = next.subscriptions.map(_.id).toSet
        val gone   = current.keys.filterNot(wanted.contains)
        ZIO.foreachDiscard(next.subscriptions) { sub =>
          current.get(sub.id) match
            case Some(row) => row.deadline.set(sub.refreshBefore)
            case None      =>
              for
                deadline <- Ref.make(sub.refreshBefore)
                keep     <- Ref.make(false)
                watched  <- Ref.make(false)
                _        <- rows.update(_ + (sub.id -> new Row(deadline, keep, watched)))
              yield ()
        } *> ZIO.foreachDiscard(gone)(id => rows.update(_ - id))
      }

    private def sleepUntil(row: Row, onExpire: UIO[Unit]): UIO[Unit] =
      def loop: UIO[Unit] =
        row.deadline.get.flatMap { deadline =>
          Clock.instant.flatMap { at =>
            val wait = deadline.toEpochMilli - at.toEpochMilli
            if wait > 0L then ZIO.sleep(Duration.fromMillis(wait)) *> loop
            else
              row.deadline.get.flatMap { again =>
                Clock.instant.flatMap { later =>
                  if again.toEpochMilli > later.toEpochMilli then loop else onExpire
                }
              }
          }
        }
      loop

    private def upsert(
        subs: Vector[StoredSubscription],
        entry: StoredSubscription,
    ): Vector[StoredSubscription] =
      if subs.exists(_.id == entry.id) then subs.map(item => if item.id == entry.id then entry else item)
      else subs :+ entry

    private def lock[E, A](body: IO[E, A]): IO[E, A] =
      permit.withPermit(body)
  end Book

  private[events] object Book:
    def make(capacity: Int, journal: Journal, initial: EventSnapshot): UIO[Book] =
      for
        snap   <- Ref.make(initial)
        rows   <- Ref.make(Map.empty[String, Row])
        permit <- Semaphore.make(1)
        closed <- Ref.make(false)
        book = new Book(capacity, journal, snap, rows, permit, closed)
        _ <- book.install(initial)
      yield book
end SubscriptionStore
