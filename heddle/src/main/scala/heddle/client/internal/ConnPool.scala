package heddle.client.internal

import heddle.client.{Authority, Client, ClientError, Target}
import heddle.http.Scheme
import zio.*

/** Whether a request may be sent again after a pooled connection turned out to be dead. */
private[heddle] enum Replay:
  case Safe, Unsafe

/** Keep-alive pool keyed by scheme and authority.
  *
  * Every exchange runs inside `acquireReleaseExitWith`, so success, failure, and interruption all settle the slot: a
  * connection goes back to the pool only when its exchange succeeded and the response allows reuse. Sockets close
  * outside `Ref` updates, which may retry.
  */
private[heddle] final class ConnPool private (
    cfg: Client.Config,
    slots: Ref[Map[ConnPool.Key, ConnPool.Slots]],
    open: Target => IO[ClientError, Http1Conn],
):
  import ConnPool.*

  /** A pooled connection the server already closed fails on first use; a `Safe` request retries once on a new one. */
  def exchange[A](target: Target, replay: Replay)(use: Http1Conn => IO[ClientError, (A, Reuse)]): IO[ClientError, A] =
    val key = Key(target.scheme, target.authority)
    attempt(key, target, Source.PoolFirst, use).catchAll {
      case Failed(_: ClientError.Io, Via.Pooled) if replay == Replay.Safe =>
        attempt(key, target, Source.FreshOnly, use).mapError(_.error)
      case failed => ZIO.fail(failed.error)
    }

  private def attempt[A](
      key: Key,
      target: Target,
      source: Source,
      use: Http1Conn => IO[ClientError, (A, Reuse)],
  ): IO[Failed, A] =
    ZIO
      .acquireReleaseExitWith(claim(key, source).mapError(Failed(_, Via.Unclaimed)))(
        (lease: Lease, exit: Exit[Failed, (A, Reuse)]) => settle(key, lease, exit)
      )(lease => lease.connection(open(target)).flatMap(use).mapError(Failed(_, lease.via)))
      .map(_._1)

  private def claim(key: Key, source: Source): IO[ClientError, Lease] =
    for
      now           <- Clock.nanoTime
      (take, stale) <- slots.modify { m =>
        val st             = m.getOrElse(key, Slots(Chunk.empty, 0))
        val (fresh, stale) = st.idle.partition(c => now - c.since < cfg.poolIdleTimeout.toNanos)
        val opened         = st.opened - stale.length
        val (take, next)   = fresh.headOption.filter(_ => source == Source.PoolFirst) match
          case Some(c)                                    => (Take.Reuse(c.conn), Slots(fresh.drop(1), opened))
          case None if opened < cfg.maxConnectionsPerHost => (Take.Open, Slots(fresh, opened + 1))
          case None                                       => (Take.Exhausted, Slots(fresh, opened))
        ((take, stale.map(_.conn)), m.updated(key, next))
      }
      _     <- ZIO.foreachDiscard(stale)(_.conn.close)
      lease <- take match
        case Take.Reuse(conn) => ZIO.succeed(Lease.Pooled(conn))
        case Take.Open        => Ref.make(Option.empty[Http1Conn]).map(Lease.Fresh(_))
        case Take.Exhausted   => ZIO.fail(ClientError.PoolExhausted(key.authority))
    yield lease

  private def settle[A](key: Key, lease: Lease, exit: Exit[Failed, (A, Reuse)]): UIO[Unit] =
    lease.current.flatMap {
      case None       => forget(key)
      case Some(conn) =>
        exit match
          case Exit.Success((_, Reuse.Keep)) => checkIn(key, conn)
          case _                             => conn.conn.close *> forget(key)
    }

  private def checkIn(key: Key, conn: Http1Conn): UIO[Unit] =
    Clock.nanoTime.flatMap { now =>
      slots
        .modify { m =>
          val st = m.getOrElse(key, Slots(Chunk.empty, 1))
          if st.idle.length >= cfg.maxIdlePerHost then (Some(conn), m.updated(key, st.copy(opened = st.opened - 1)))
          else (None, m.updated(key, st.copy(idle = st.idle :+ Idle(conn, now))))
        }
        .flatMap(ZIO.foreachDiscard(_)(_.conn.close))
    }

  private def forget(key: Key): UIO[Unit] =
    slots.update { m =>
      val st = m.getOrElse(key, Slots(Chunk.empty, 1))
      m.updated(key, st.copy(opened = math.max(0, st.opened - 1)))
    }

  private def evictExpired: UIO[Unit] =
    Clock.nanoTime.flatMap { now =>
      slots
        .modify { m =>
          val live    = (c: Idle) => now - c.since < cfg.poolIdleTimeout.toNanos
          val expired = m.values.flatMap(_.idle.filterNot(live)).map(_.conn).toList
          val next    = m.flatMap { (key, st) =>
            val keep   = st.idle.filter(live)
            val opened = st.opened - (st.idle.length - keep.length)
            Option.when(keep.nonEmpty || opened > 0)(key -> Slots(keep, opened))
          }
          (expired, next)
        }
        .flatMap(ZIO.foreachDiscard(_)(_.conn.close))
    }

  private def closeIdle: UIO[Unit] =
    slots.getAndSet(Map.empty).flatMap(m => ZIO.foreachDiscard(m.values.flatMap(_.idle))(_.conn.conn.close))
end ConnPool

private[heddle] object ConnPool:
  def scoped(cfg: Client.Config, open: Target => IO[ClientError, Http1Conn]): URIO[Scope, ConnPool] =
    for
      slots <- Ref.make(Map.empty[Key, Slots])
      pool = new ConnPool(cfg, slots, open)
      _ <- ZIO.addFinalizer(pool.closeIdle)
      _ <- (ZIO.sleep(evictTick(cfg)) *> pool.evictExpired).forever.forkScoped
    yield pool

  private def evictTick(cfg: Client.Config): Duration =
    if cfg.poolIdleTimeout == Duration.Infinity || cfg.poolIdleTimeout.toNanos <= 0L then 1.second
    else cfg.poolIdleTimeout.min(1.second).max(10.millis)

  private final case class Key(scheme: Scheme, authority: Authority)
  private final case class Idle(conn: Http1Conn, since: Long)
  private final case class Slots(idle: Chunk[Idle], opened: Int)
  private final case class Failed(error: ClientError, via: Via)

  private enum Via:
    case Pooled, Fresh, Unclaimed

  /** Where a claim may take its connection from. The retry after a dead pooled connection skips the pool. */
  private enum Source:
    case PoolFirst, FreshOnly

  private enum Take:
    case Reuse(conn: Http1Conn)
    case Open
    case Exhausted

  private enum Lease:
    /** An idle connection taken from the pool. */
    case Pooled(conn: Http1Conn)

    /** Room to open a connection; the cell records it once `open` succeeds so release can close it. */
    case Fresh(cell: Ref[Option[Http1Conn]])

    def via: Via =
      this match
        case Pooled(_) => Via.Pooled
        case Fresh(_)  => Via.Fresh

    def connection(open: IO[ClientError, Http1Conn]): IO[ClientError, Http1Conn] =
      this match
        case Pooled(conn) => ZIO.succeed(conn)
        case Fresh(cell)  => ZIO.uninterruptibleMask(restore => restore(open).tap(c => cell.set(Some(c))))

    def current: UIO[Option[Http1Conn]] =
      this match
        case Pooled(conn) => ZIO.some(conn)
        case Fresh(cell)  => cell.get
  end Lease
end ConnPool
