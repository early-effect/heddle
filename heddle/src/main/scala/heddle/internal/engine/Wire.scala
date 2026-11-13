package heddle.internal.engine

import heddle.internal.duplex.Sink
import zio.*

/** One connection's bytes: where they come from, where they go, and whether they came over TLS. */
private[heddle] final case class Wire(src: ConnBuf, send: Sink, secure: Boolean)

/** The server's intake switch and this connection's in-flight mark. Halt and the connection are different fibers, and
  * both touches are effects, so these are `Ref` and a function into the server's in-flight set. A `FiberRef` would not
  * be visible to halt.
  */
private[heddle] final case class Lifecycle(takingWork: Ref[Boolean], busy: Boolean => UIO[Unit])

private[heddle] object Lifecycle:

  /** A connection that is not in a server's in-flight set. Protocol tests use it. */
  def detached: UIO[Lifecycle] =
    Ref.make(true).map(Lifecycle(_, _ => ZIO.unit))
end Lifecycle
