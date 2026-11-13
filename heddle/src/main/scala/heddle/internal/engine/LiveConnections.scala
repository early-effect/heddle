package heddle.internal.engine

import heddle.internal.duplex.{AcceptError, ByteConn, Listener}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.jdk.CollectionConverters.*
import zio.*

/** A server's open connections, on every platform: admission up to `maxConnections`, and the halt that closes idle
  * connections, waits for busy ones up to a grace period, then closes and interrupts the rest.
  *
  * `takingWork` and the in-flight set are `Ref`s. Halt and the connection fibers are different fibers, and every read
  * or write is already an effect, at a request boundary. A `FiberRef` would not be visible to halt.
  */
private[heddle] final class LiveConnections private (
    maxConnections: Int,
    val takingWork: Ref[Boolean],
    inflight: Ref[(Set[Long], List[Promise[Nothing, Unit]])],
):
  private val open  = ConcurrentHashMap[Long, LiveConnections.Entry]()
  private val ids   = AtomicLong(0)
  private val count = AtomicInteger(0)

  /** Accepts until the listener closes. Each connection runs `serve` on its own fiber, which `halt` supervises.
    * `busy(true)` marks that connection in flight. `busy(false)` clears it and wakes halt when none remain.
    */
  def acceptAll[R, E, C <: ByteConn](listener: Listener[C])(
      serve: (C, Boolean => UIO[Unit]) => ZIO[R, E, Unit]
  ): URIO[R, Unit] =
    listener.accept.foldZIO(
      {
        case AcceptError.Closed        => ZIO.unit
        case AcceptError.Failed(cause) => ZIO.logWarning(s"accept failed: $cause") *> acceptAll(listener)(serve)
      },
      conn => admit(conn, serve) *> acceptAll(listener)(serve),
    )

  private def admit[R, E, C <: ByteConn](
      conn: C,
      serve: (C, Boolean => UIO[Unit]) => ZIO[R, E, Unit],
  ): URIO[R, Unit] =
    ZIO.suspendSucceed {
      if count.incrementAndGet() > maxConnections then ZIO.succeed(count.decrementAndGet()) *> conn.close
      else
        val id    = ids.incrementAndGet()
        val entry = LiveConnections.Entry(id, conn, AtomicReference(Option.empty))
        val busy  = (up: Boolean) => mark(id, up)
        open.put(id, entry)
        serve(conn, busy)
          .ensuring(
            ZIO.succeed(open.remove(id)) *> busy(false) *> ZIO.succeed(count.decrementAndGet()) *> conn.close
          )
          .forkDaemon
          .flatMap(fiber => ZIO.succeed(entry.fiber.set(Some(fiber))))
    }

  /** A scope finalizer, so uninterruptible. The grace sleep runs on its own fiber and completes the same promise the
    * idle transition completes, so the wait does not depend on interrupting this finalizer, and an idle server does not
    * sleep.
    */
  def halt(listener: Listener[ByteConn], grace: Duration): UIO[Unit] =
    takingWork.set(false) *>
      listener.close *>
      inflight.get.flatMap { case (busy, _) =>
        entries.flatMap(es => ZIO.foreachDiscard(es.filterNot(e => busy.contains(e.id)))(_.conn.close))
      } *>
      waitUntilIdle(grace) *>
      entries.flatMap(es => ZIO.foreachDiscard(es)(_.conn.close)) *>
      entries.flatMap(es => ZIO.foreachParDiscard(es.flatMap(_.fiber.get()))(_.interrupt))

  private def entries: UIO[List[LiveConnections.Entry]] =
    ZIO.succeed(open.values.asScala.toList)

  /** One step owns the set and the waiters, so a transition to idle cannot pass a waiter that has not registered.
    *
    * A `true` after the entry has left `open` is ignored. The connection fiber removes itself before its final
    * `busy(false)`, and a stream fiber can still report itself busy after that. Putting the id back would make halt
    * wait out the grace for a connection it can no longer close.
    */
  private def mark(id: Long, up: Boolean): UIO[Unit] =
    inflight
      .modify { case (busy, waiters) =>
        val next = if up && open.containsKey(id) then busy + id else busy - id
        if next.isEmpty && waiters.nonEmpty then (waiters, (next, Nil))
        else (Nil, (next, waiters))
      }
      .flatMap(wake => ZIO.foreachDiscard(wake)(_.succeed(())))

  private def waitUntilIdle(grace: Duration): UIO[Unit] =
    if grace.isZero then ZIO.unit
    else
      Promise.make[Nothing, Unit].flatMap { done =>
        inflight
          .modify { case (open, waiters) =>
            if open.isEmpty then (true, (open, waiters))
            else (false, (open, done :: waiters))
          }
          .flatMap { idle =>
            if idle then ZIO.unit
            else
              // The finalizer is uninterruptible, and a fork copies that. The grace fiber has to be
              // interruptible or joining it waits out the whole grace after the connection goes idle.
              (ZIO.sleep(grace) *> done.succeed(())).interruptible.forkDaemon.flatMap { graceFiber =>
                done.await *> graceFiber.interrupt.unit
              }
          }
      }
end LiveConnections

private[heddle] object LiveConnections:
  def make(maxConnections: Int): UIO[LiveConnections] =
    for
      taking   <- Ref.make(true)
      inflight <- Ref.make((Set.empty[Long], List.empty[Promise[Nothing, Unit]]))
    yield LiveConnections(maxConnections, taking, inflight)

  private final case class Entry(
      id: Long,
      conn: ByteConn,
      fiber: AtomicReference[Option[Fiber[Any, Any]]],
  )
end LiveConnections
