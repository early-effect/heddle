package heddle.internal.posix

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import zio.*

/** One dedicated poller thread. Same shape as libuv: block in `poll`, then complete the `ZIO.async` waiters. Native
  * does not link libuv.
  */
private[heddle] object Poller:
  enum Interest:
    case Read, Write

  def readable(fd: Int): Task[Unit] = await(fd, Interest.Read)

  def writable(fd: Int): Task[Unit] = await(fd, Interest.Write)

  def await(fd: Int, interest: Interest): Task[Unit] =
    start()
    ZIO.asyncInterrupt[Any, Throwable, Unit] { complete =>
      val waiter = Waiter(interest, complete)
      register(fd, waiter)
      Left(ZIO.succeed(unregister(fd, waiter)))
    }

  private final class Waiter(val interest: Interest, val complete: Task[Unit] => Unit)

  private val waiters = new ConcurrentHashMap[Integer, CopyOnWriteArrayList[Waiter]]()
  private val running = new AtomicBoolean(false)
  private val wakeR   = Array(-1)
  private val wakeW   = Array(-1)

  private def start(): Unit =
    if running.compareAndSet(false, true) then
      Zone {
        val fds = alloc[CInt](2)
        if unistd.pipe(fds) != 0 then
          running.set(false)
          throw java.io.IOException("poller pipe")
        wakeR(0) = !fds
        wakeW(0) = !(fds + 1)
        Net.setNonBlocking(wakeR(0))
        Net.setNonBlocking(wakeW(0))
      }
      val t = new Thread(() => loop(), "heddle-poller")
      t.setDaemon(true)
      t.start()

  private def register(fd: Int, waiter: Waiter): Unit =
    waiters.computeIfAbsent(fd, _ => new CopyOnWriteArrayList[Waiter]()).add(waiter)
    wake()

  private def unregister(fd: Int, waiter: Waiter): Unit =
    Option(waiters.get(fd)).foreach { list =>
      list.remove(waiter)
      if list.isEmpty then waiters.remove(fd, list)
    }

  private def wake(): Unit =
    val w = wakeW(0)
    if w >= 0 then
      val one = Array[Byte](1)
      val _   = unistd.write(w, one.at(0), 1.toUSize)

  private def drainWake(): Unit =
    val buf = new Array[Byte](64)
    while Net.read(wakeR(0), buf, 0, buf.length) > 0 do ()

  private def loop(): Unit =
    while running.get do
      val keys = waiters.keys()
      val fds  = scala.collection.mutable.ArrayBuffer.empty[Int]
      fds += wakeR(0)
      while keys.hasMoreElements do fds += keys.nextElement()
      val ready = Net.pollReady(fds.toArray, -1)
      if ready.contains(wakeR(0)) then drainWake()
      ready.foreach { fd =>
        if fd != wakeR(0) then fire(fd)
      }

  private def fire(fd: Int): Unit =
    val readable = Net.pollIn(fd, 0)
    val writable = Net.pollOut(fd, 0)
    Option(waiters.get(fd)).foreach { list =>
      val it = list.iterator()
      while it.hasNext do
        val w  = it.next()
        val ok = w.interest match
          case Interest.Read  => readable
          case Interest.Write => writable
        if ok then
          list.remove(w)
          w.complete(ZIO.unit)
      if list.isEmpty then waiters.remove(fd, list)
    }
  end fire
end Poller
