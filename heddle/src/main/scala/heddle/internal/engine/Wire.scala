package heddle.internal.engine

import heddle.internal.duplex.Sink
import java.util.concurrent.atomic.AtomicBoolean

/** One connection's bytes: where they come from, where they go, and whether they came over TLS. */
private[heddle] final case class Wire(src: ConnBuf, send: Sink, secure: Boolean)

/** The server's intake switch and this connection's in-flight flag. Graceful shutdown reads both from outside the
  * connection's fiber, so they are atomics.
  */
private[heddle] final case class Lifecycle(takingWork: AtomicBoolean, busy: AtomicBoolean)
