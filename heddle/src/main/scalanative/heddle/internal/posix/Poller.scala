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
  def await(fd: Int, interest: Interest): IO[NetError, Unit] =
    ZIO.fromEither(start()) *>
      ZIO.asyncInterrupt[Any, NetError, Unit] { complete =>
        val waiter = Waiter(interest, complete)
        register(fd, waiter)
        Left(ZIO.succeed(unregister(fd, waiter)))
      }

  private final class Waiter(val interest: Interest, val complete: IO[NetError, Unit] => Unit):
    val events: Int = interest match
      case Interest.Read  => Net.PollIn
      case Interest.Write => Net.PollOut

  private val waiters = new ConcurrentHashMap[Integer, CopyOnWriteArrayList[Waiter]]()
  private val running = new AtomicBoolean(false)
  private val wakeR   = Array(-1)
  private val wakeW   = Array(-1)

  private def start(): Either[NetError, Unit] =
    if !running.compareAndSet(false, true) then Right(())
    else
      Net.pipe() match
        case Left(e) =>
          running.set(false)
          Left(e)
        case Right((r, w)) =>
          wakeR(0) = r
          wakeW(0) = w
          val t = new Thread(() => loop(), "heddle-poller")
          t.setDaemon(true)
          t.start()
          Right(())

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
    while Net.read(wakeR(0), buf, 0, buf.length).exists { case Transfer.Moved(n) => n > 0; case _ => false } do ()

  /** Each fd polls for the union of its waiters' interests; the wake pipe comes first. */
  private def loop(): Unit =
    while running.get do
      val fds    = scala.collection.mutable.ArrayBuffer(wakeR(0))
      val events = scala.collection.mutable.ArrayBuffer(Net.PollIn)
      waiters.forEach { (fd, list) =>
        fds += fd.intValue
        var bits = 0
        list.forEach(w => bits |= w.events)
        events += bits
      }
      Net.pollReady(fds.toArray, events.toArray, -1) match
        case Left(e)        => failAll(e)
        case Right(revents) =>
          if revents(0) != 0 then drainWake()
          var i = 1
          while i < fds.length do
            if revents(i) != 0 then fire(fds(i), revents(i))
            i += 1
      end match
    end while
  end loop

  private def fire(fd: Int, revents: Int): Unit =
    Option(waiters.get(fd)).foreach { list =>
      list.forEach { w =>
        if (revents & (w.events | Net.PollFailed)) != 0 then
          list.remove(w)
          w.complete(ZIO.unit)
      }
      if list.isEmpty then waiters.remove(fd, list)
    }

  /** `poll` itself failed (out of memory, say): every parked fiber fails with it instead of waiting forever. */
  private def failAll(e: NetError): Unit =
    waiters.forEach { (fd, list) =>
      list.forEach(w => w.complete(ZIO.fail(e)))
      waiters.remove(fd, list)
    }
end Poller
