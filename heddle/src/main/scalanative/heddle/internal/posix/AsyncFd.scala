package heddle.internal.posix

import zio.*

/** Park a fiber until the poller thread says `fd` is ready.
  *
  * Same shape as Node `ZIO.async` plus `socket.on("connection")`. The callback is ours: one `poll` thread, then
  * complete the waiter. That is libuv's loop without linking libuv.
  */
private[heddle] object AsyncFd:
  def readable(fd: Int): IO[NetError, Unit] = Poller.await(fd, Interest.Read)

  def writable(fd: Int): IO[NetError, Unit] = Poller.await(fd, Interest.Write)

  def ready(fd: Int, on: Interest): IO[NetError, Unit] = Poller.await(fd, on)
