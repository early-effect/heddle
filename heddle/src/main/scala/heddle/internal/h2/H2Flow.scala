package heddle.internal.h2

import zio.*
import zio.stm.*

/** RFC 9113 §6.9 flow control for one connection. Windows live in STM, so a sender waits for exactly its own credit and
  * a window update wakes exactly the senders it unblocks.
  */
private[heddle] final class H2Flow(
    connSend: TRef[Long],
    connRecv: TRef[Long],
    streamSend: TMap[Int, Long],
    streamRecv: TMap[Int, Long],
    sendInitial: TRef[Long],
    recvInitial: Long,
):
  def open(id: Int): UIO[Unit] =
    (sendInitial.get.flatMap(streamSend.put(id, _)) *> streamRecv.put(id, recvInitial)).commit

  def close(id: Int): UIO[Unit] =
    (streamSend.delete(id) *> streamRecv.delete(id)).commit

  /** Accounts for `n` received bytes; `false` when they overrun the connection or stream window. */
  def takeRecv(id: Int, n: Int): UIO[Boolean] =
    if n <= 0 then ZIO.succeed(true)
    else
      (connRecv.get <*> streamRecv.getOrElse(id, 0L)).flatMap { (cw, sw) =>
        if cw < n || sw < n then STM.succeed(false)
        else connRecv.update(_ - n) *> streamRecv.put(id, sw - n).as(true)
      }.commit

  def restoreRecv(id: Int, n: Int): UIO[Unit] =
    if n <= 0 then ZIO.unit
    else (connRecv.update(_ + n) *> streamRecv.updateWith(id)(_.map(_ + n))).commit.unit

  /** Waits until both windows are open, then takes up to `want` bytes of credit: a window smaller than a frame still
    * moves data. `None` when the stream closed while it waited.
    */
  def takeSend(id: Int, want: Int): UIO[Option[Int]] =
    if want <= 0 then ZIO.some(0)
    else
      streamSend
        .get(id)
        .flatMap {
          case None     => STM.none
          case Some(sw) =>
            connSend.get.flatMap { cw =>
              val take = math.min(math.min(cw, sw), want.toLong)
              STM.check(take > 0) *> connSend.update(_ - take) *> streamSend.put(id, sw - take).as(Some(take.toInt))
            }
        }
        .commit

  /** A WINDOW_UPDATE. `false` when it would push a window past 2^31-1 (RFC 9113 §6.9.1). */
  def creditSend(id: Int, n: Int): UIO[Boolean] =
    if n <= 0 then ZIO.succeed(true)
    else if id == 0 then connSend.modify(w => if w + n > H2Flow.MaxWindow then (false, w) else (true, w + n)).commit
    else
      streamSend
        .get(id)
        .flatMap {
          case None                                => STM.succeed(true)
          case Some(w) if w + n > H2Flow.MaxWindow => STM.succeed(false)
          case Some(w)                             => streamSend.put(id, w + n).as(true)
        }
        .commit

  /** The peer's SETTINGS_INITIAL_WINDOW_SIZE changed: every open stream's window moves by the difference (§6.9.2). */
  def resizeSend(initial: Long): UIO[Unit] =
    sendInitial
      .getAndSet(initial)
      .flatMap { old =>
        streamSend.transformValues(_ + (initial - old))
      }
      .commit
end H2Flow

private[heddle] object H2Flow:
  val ConnectionInitialWindow: Long = 65535
  val MaxWindow: Long               = Int.MaxValue.toLong
  val FlowControlError: Int         = 0x3
  val RefusedStream: Int            = 0x7

  /** `recvInitial` is what this server advertised; the peer's initial send window is 65,535 until its SETTINGS say
    * otherwise.
    */
  def make(recvInitial: Int): UIO[H2Flow] =
    (for
      connSend   <- TRef.make(ConnectionInitialWindow)
      connRecv   <- TRef.make(ConnectionInitialWindow)
      streamSend <- TMap.empty[Int, Long]
      streamRecv <- TMap.empty[Int, Long]
      initial    <- TRef.make(ConnectionInitialWindow)
    yield H2Flow(connSend, connRecv, streamSend, streamRecv, initial, recvInitial.toLong)).commit
end H2Flow
