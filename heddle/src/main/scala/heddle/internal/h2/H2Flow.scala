package heddle.internal.h2

import zio.*

/** Connection window starts at 65535 (RFC 7540). Stream windows start at SETTINGS_INITIAL_WINDOW_SIZE. */
private[heddle] final class H2Flow(
    connSend: Ref[Int],
    connRecv: Ref[Int],
    streamSend: Ref[Map[Int, Int]],
    streamRecv: Ref[Map[Int, Int]],
    pulse: Queue[Unit],
    streamInitial: Int,
):
  def open(id: Int): UIO[Unit] =
    streamSend.update(_ + (id -> streamInitial)) *>
      streamRecv.update(_ + (id -> streamInitial))

  def close(id: Int): UIO[Unit] =
    streamSend.update(_ - id) *> streamRecv.update(_ - id)

  def takeRecv(id: Int, n: Int): UIO[Boolean] =
    if n <= 0 then ZIO.succeed(true)
    else
      connRecv.get.zip(streamRecv.get.map(_.getOrElse(id, 0))).flatMap { (cw, sw) =>
        if cw < n || sw < n then ZIO.succeed(false)
        else connRecv.update(_ - n) *> streamRecv.update(_.updatedWith(id)(_.map(_ - n))) *> ZIO.succeed(true)
      }

  def restoreRecv(id: Int, n: Int): UIO[Unit] =
    if n <= 0 then ZIO.unit
    else connRecv.update(_ + n) *> streamRecv.update(_.updatedWith(id)(_.map(_ + n)))

  def takeSend(id: Int, n: Int): UIO[Unit] =
    if n <= 0 then ZIO.unit
    else
      def loop: UIO[Unit] =
        connSend.get.zip(streamSend.get.map(_.getOrElse(id, 0))).flatMap { (cw, sw) =>
          if cw >= n && sw >= n then
            connSend.update(_ - n) *> streamSend.update(_.updatedWith(id)(_.map(_ - n).orElse(Some(0))))
          else pulse.take *> loop
        }
      loop

  def creditSend(id: Int, n: Int): UIO[Unit] =
    if n <= 0 then ZIO.unit
    else
      val bump =
        if id == 0 then connSend.update(_ + n)
        else streamSend.update(_.updatedWith(id)(cur => Some(cur.getOrElse(0) + n)))
      bump *> pulse.offer(()).unit
end H2Flow

private[heddle] object H2Flow:
  val ConnectionInitialWindow: Int = 65535
  val FlowControlError: Int        = 0x3
  val RefusedStream: Int           = 0x7

  def make(streamInitial: Int): UIO[H2Flow] =
    for
      connSend   <- Ref.make(ConnectionInitialWindow)
      connRecv   <- Ref.make(ConnectionInitialWindow)
      streamSend <- Ref.make(Map.empty[Int, Int])
      streamRecv <- Ref.make(Map.empty[Int, Int])
      pulse      <- Queue.unbounded[Unit]
    yield H2Flow(connSend, connRecv, streamSend, streamRecv, pulse, streamInitial)
end H2Flow
