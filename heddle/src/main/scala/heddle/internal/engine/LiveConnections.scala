package heddle.internal.engine

import heddle.internal.duplex.{AcceptError, ByteConn, Listener}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.jdk.CollectionConverters.*
import zio.*

/** A server's open connections, on every platform: admission up to `maxConnections`, and the halt that closes idle
  * connections, waits for busy ones up to a grace period, then closes and interrupts the rest.
  */
private[heddle] final class LiveConnections private (maxConnections: Int):
  /** Cleared by `halt`. A connection checks it before reading its next request. */
  val takingWork: AtomicBoolean = AtomicBoolean(true)

  private val open  = ConcurrentHashMap[Long, LiveConnections.Entry]()
  private val ids   = AtomicLong(0)
  private val count = AtomicInteger(0)

  /** Accepts until the listener closes. Each connection runs `serve` on its own fiber, which `halt` supervises. */
  def acceptAll[R, E](listener: Listener)(serve: (ByteConn, AtomicBoolean) => ZIO[R, E, Unit]): URIO[R, Unit] =
    listener.accept.foldZIO(
      {
        case AcceptError.Closed        => ZIO.unit
        case AcceptError.Failed(cause) => ZIO.logWarning(s"accept failed: $cause") *> acceptAll(listener)(serve)
      },
      conn => admit(conn, serve) *> acceptAll(listener)(serve),
    )

  private def admit[R, E](conn: ByteConn, serve: (ByteConn, AtomicBoolean) => ZIO[R, E, Unit]): URIO[R, Unit] =
    ZIO.suspendSucceed {
      if count.incrementAndGet() > maxConnections then ZIO.succeed(count.decrementAndGet()) *> conn.close
      else
        val id    = ids.incrementAndGet()
        val entry = LiveConnections.Entry(conn, AtomicBoolean(false), AtomicReference(Option.empty))
        open.put(id, entry)
        serve(conn, entry.busy)
          .ensuring(ZIO.succeed { open.remove(id); count.decrementAndGet() } *> conn.close)
          .forkDaemon
          .flatMap(fiber => ZIO.succeed(entry.fiber.set(Some(fiber))))
    }

  /** A Scope finalizer, so uninterruptible: the grace wait is `Schedule.upTo`, not `timeout`. */
  def halt(listener: Listener, grace: Duration): UIO[Unit] =
    ZIO.succeed(takingWork.set(false)) *>
      listener.close *>
      entries.flatMap(es => ZIO.foreachDiscard(es.filterNot(_.busy.get()))(_.conn.close)) *>
      waitUntilIdle(grace) *>
      entries.flatMap(es => ZIO.foreachDiscard(es)(_.conn.close)) *>
      entries.flatMap(es => ZIO.foreachParDiscard(es.flatMap(_.fiber.get()))(_.interrupt))

  private def entries: UIO[List[LiveConnections.Entry]] =
    ZIO.succeed(open.values.asScala.toList)

  private def waitUntilIdle(grace: Duration): UIO[Unit] =
    if grace.isZero then ZIO.unit
    else
      entries
        .map(_.exists(_.busy.get()))
        .repeat(Schedule.spaced(10.millis) && Schedule.recurWhile[Boolean](identity) && Schedule.upTo(grace))
        .unit
end LiveConnections

private[heddle] object LiveConnections:
  def make(maxConnections: Int): UIO[LiveConnections] =
    ZIO.succeed(LiveConnections(maxConnections))

  private final case class Entry(
      conn: ByteConn,
      busy: AtomicBoolean,
      fiber: AtomicReference[Option[Fiber[Any, Any]]],
  )
end LiveConnections
