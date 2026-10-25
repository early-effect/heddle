package heddle.internal.engine

import zio.*

/** A blocking syscall on this fiber's thread when that thread is virtual.
  *
  * `attemptBlocking` always shifts to the blocking executor. `attemptBlockingInterrupt` also `forkDaemon`s the syscall
  * (ZIO 2.1). A Loom connection fiber is already a virtual thread, so that shift is a second hop. A platform worker
  * still shifts, and a JVM that could not enable Loom does not pin a ZIO worker.
  */
private[heddle] object Carrier:
  def inline[A](effect: => A): Task[A] =
    ZIO.suspendSucceed {
      if Thread.currentThread.isVirtual then ZIO.attempt(effect)
      else ZIO.attemptBlocking(effect)
    }
