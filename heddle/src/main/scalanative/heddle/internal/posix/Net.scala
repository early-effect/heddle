package heddle.internal.posix

import scala.scalanative.posix.arpa.inet.*
import scala.scalanative.posix.errno.{EAGAIN, EINPROGRESS, EINTR, EWOULDBLOCK, errno}
import scala.scalanative.posix.fcntl
import scala.scalanative.posix.netdb
import scala.scalanative.posix.netdbOps.*
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.netinet.tcp.*
import scala.scalanative.posix.poll
import scala.scalanative.posix.pollOps.*
import scala.scalanative.posix.signal as psignal
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import zio.Chunk

/** A socket address and its family, detached from `getaddrinfo`'s list. */
private[heddle] final class SockAddr(val family: Int, private[posix] val bytes: Array[Byte])

/** posixlib's `getaddrinfo` glue, marked blocking so a slow resolver does not stall the multithreaded GC. */
@extern
private object Gai:
  @blocking
  @name("scalanative_getaddrinfo")
  def getaddrinfo(
      name: CString,
      service: CString,
      hints: Ptr[netdb.addrinfo],
      res: Ptr[Ptr[netdb.addrinfo]],
  ): CInt = extern
end Gai

/** POSIX sockets. `Ptr` stays in this package. */
private[heddle] object Net:
  /** A write to a peer that already closed raises SIGPIPE, and its default action kills the process. Every socket write
    * here checks its result, so the signal is ignored once, before the first socket opens, and the write fails with
    * `EPIPE` instead. OpenSSL writes to the same sockets, so a per-call `MSG_NOSIGNAL` would not cover it.
    */
  private lazy val sigpipeIgnored: Unit =
    val _ = psignal.signal(psignal.SIGPIPE, psignal.SIG_IGN)

  def listen(host: String, port: Int, backlog: Int, reuse: Boolean): Either[NetError, Int] =
    sigpipeIgnored
    withSocket(socket.AF_INET) { fd =>
      Zone {
        for
          _    <- if reuse then setInt(fd, socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) else Right(())
          _    <- setNonBlocking(fd)
          addr <- ipv4(host, port)
          _ <- check(socket.bind(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], sizeof[sockaddr_in].toUInt), Syscall.Bind)
          _ <- check(socket.listen(fd, backlog), Syscall.Listen)
        yield ()
      }
    }
  end listen

  /** Starts a non-blocking connect to an IPv4 literal. Wait for `AsyncFd.writable`, then read `socketError`. */
  def connect(host: String, port: Int): Either[NetError, Int] =
    sigpipeIgnored
    withSocket(socket.AF_INET) { fd =>
      Zone {
        for
          _    <- setNonBlocking(fd)
          addr <- ipv4(host, port)
          _    <- started(socket.connect(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], sizeof[sockaddr_in].toUInt))
        yield ()
      }
    }
  end connect

  /** Starts a non-blocking connect. Wait for `AsyncFd.writable`, then read `socketError`. */
  def connect(addr: SockAddr): Either[NetError, Int] =
    sigpipeIgnored
    withSocket(addr.family) { fd =>
      setNonBlocking(fd).flatMap { _ =>
        started(socket.connect(fd, addr.bytes.at(0).asInstanceOf[Ptr[socket.sockaddr]], addr.bytes.length.toUInt))
      }
    }

  /** Blocking `getaddrinfo`. Each address is copied out so it outlives the call. */
  def resolve(host: String, port: Int): Either[NetError, Chunk[SockAddr]] =
    Zone {
      val hints = alloc[netdb.addrinfo]()
      hints.ai_socktype = socket.SOCK_STREAM
      val res = alloc[Ptr[netdb.addrinfo]]()
      val rc  = Gai.getaddrinfo(toCString(host), toCString(port.toString), hints, res)
      if rc != 0 then Left(NetError.Unresolved(host, fromCString(netdb.gai_strerror(rc))))
      else
        val out = Chunk.newBuilder[SockAddr]
        var ai  = !res
        while ai != null do
          val len   = ai.ai_addrlen.toInt
          val bytes = new Array[Byte](len)
          val src   = ai.ai_addr.asInstanceOf[Ptr[Byte]]
          var i     = 0
          while i < len do
            bytes(i) = src(i)
            i += 1
          out += SockAddr(ai.ai_family, bytes)
          ai = ai.ai_next
        end while
        netdb.freeaddrinfo(!res)
        Right(out.result())
      end if
    }

  /** The pending error of a connect that `AsyncFd.writable` says has finished; `0` is success. */
  def socketError(fd: Int): Either[NetError, Int] =
    Zone {
      val v   = alloc[CInt]()
      val len = alloc[socket.socklen_t]()
      !v = 0
      !len = sizeof[CInt].toUInt
      val rc = socket.getsockopt(fd, socket.SOL_SOCKET, socket.SO_ERROR, v.asInstanceOf[Ptr[Byte]], len)
      check(rc, Syscall.SocketError).map(_ => !v)
    }

  /** An accepted, non-blocking descriptor, or `None` when no connection is waiting. */
  def accept(listenFd: Int): Either[NetError, Option[Int]] =
    Zone {
      val addr    = alloc[sockaddr_in]()
      val addrlen = alloc[socket.socklen_t]()
      !addrlen = sizeof[sockaddr_in].toUInt
      val fd = socket.accept(listenFd, addr.asInstanceOf[Ptr[socket.sockaddr]], addrlen)
      if fd >= 0 then
        setNonBlocking(fd) match
          case Left(e)  => close(fd); Left(e)
          case Right(_) => Right(Some(fd))
      else if wouldBlock then Right(None)
      else Left(failed(Syscall.Accept))
    }

  /** Blocks in `poll` until one of `fds` is ready for its `events`, and returns each fd's `revents`. */
  def pollReady(fds: Array[Int], events: Array[Int], timeoutMs: Int): Either[NetError, Array[Int]] =
    Zone {
      val pfds = alloc[poll.struct_pollfd](fds.length.max(1))
      var i    = 0
      while i < fds.length do
        val p = pfds + i
        p.fd = fds(i)
        p.events = events(i).toShort
        p.revents = 0.toShort
        i += 1
      val n = poll.poll(pfds, fds.length.toUInt, timeoutMs)
      if n < 0 && errno == EINTR then pollReady(fds, events, timeoutMs)
      else if n < 0 then Left(failed(Syscall.Poll))
      else Right(Array.tabulate(fds.length)(k => (pfds + k).revents.toInt & 0xffff))
    }

  val PollIn: Int  = poll.POLLIN
  val PollOut: Int = poll.POLLOUT

  /** Readiness that wakes every waiter, so each one sees the failure on its next call. */
  val PollFailed: Int = poll.POLLHUP | poll.POLLERR | poll.POLLNVAL

  def localPort(fd: Int): Int =
    Zone {
      val addr    = alloc[sockaddr_in]()
      val addrlen = alloc[socket.socklen_t]()
      !addrlen = sizeof[sockaddr_in].toUInt
      if socket.getsockname(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], addrlen) != 0 then 0
      else ntohPort(addr.sin_port)
    }

  def close(fd: Int): Unit =
    if fd >= 0 then
      val _ = unistd.close(fd)

  def read(fd: Int, dst: Array[Byte], off: Int, len: Int): Either[NetError, Transfer] =
    if len <= 0 then Right(Transfer.Moved(0))
    else
      val n = unistd.read(fd, dst.at(off), len.toUSize).toLong
      if n > 0 then Right(Transfer.Moved(n.toInt))
      else if n == 0 then Right(Transfer.Eof)
      else if errno == EINTR then read(fd, dst, off, len)
      else if wouldBlock then Right(Transfer.Blocked(Interest.Read))
      else Left(failed(Syscall.Read))

  def write(fd: Int, src: Array[Byte], off: Int, len: Int): Either[NetError, Transfer] =
    if len <= 0 then Right(Transfer.Moved(0))
    else
      val n = unistd.write(fd, src.at(off), len.toUSize).toLong
      if n > 0 then Right(Transfer.Moved(n.toInt))
      else if n == 0 then Right(Transfer.Blocked(Interest.Write))
      else if errno == EINTR then write(fd, src, off, len)
      else if wouldBlock then Right(Transfer.Blocked(Interest.Write))
      else Left(failed(Syscall.Write))

  def setTcpNoDelay(fd: Int, on: Boolean): Either[NetError, Unit] =
    setInt(fd, IPPROTO_TCP, TCP_NODELAY, if on then 1 else 0)

  def setKeepAlive(fd: Int, on: Boolean): Either[NetError, Unit] =
    setInt(fd, socket.SOL_SOCKET, socket.SO_KEEPALIVE, if on then 1 else 0)

  def setNonBlocking(fd: Int): Either[NetError, Unit] =
    val flags = fcntl.fcntl(fd, fcntl.F_GETFL, 0)
    if flags < 0 then Left(failed(Syscall.GetFlags))
    else check(fcntl.fcntl(fd, fcntl.F_SETFL, flags | fcntl.O_NONBLOCK), Syscall.SetFlags)

  /** A pipe whose two ends are non-blocking: `(read, write)`. */
  def pipe(): Either[NetError, (Int, Int)] =
    Zone {
      val fds = alloc[CInt](2)
      if unistd.pipe(fds) != 0 then Left(failed(Syscall.Pipe))
      else
        val (r, w) = (!fds, !(fds + 1))
        setNonBlocking(r).flatMap(_ => setNonBlocking(w)) match
          case Left(e)  => close(r); close(w); Left(e)
          case Right(_) => Right((r, w))
    }

  /** Opens a socket and runs `setup` on it; the socket is closed unless `setup` succeeds. */
  private def withSocket(family: Int)(setup: Int => Either[NetError, Unit]): Either[NetError, Int] =
    val fd = socket.socket(family, socket.SOCK_STREAM, 0)
    if fd < 0 then Left(failed(Syscall.Socket))
    else
      setup(fd) match
        case Left(e)  => close(fd); Left(e)
        case Right(_) => Right(fd)

  private def wouldBlock: Boolean =
    errno == EAGAIN || errno == EWOULDBLOCK

  /** A non-blocking connect that has not finished yet is started, not failed. */
  private def started(rc: CInt): Either[NetError, Unit] =
    if rc == 0 || errno == EINPROGRESS || wouldBlock then Right(()) else Left(failed(Syscall.Connect))

  private def check(rc: CInt, call: Syscall): Either[NetError, Unit] =
    if rc < 0 then Left(failed(call)) else Right(())

  /** Reads `errno` now, before a cleanup call can overwrite it. */
  private def failed(call: Syscall): NetError = NetError.Failed(call, errno)

  private def setInt(fd: Int, level: CInt, opt: CInt, value: Int): Either[NetError, Unit] =
    Zone {
      val v = alloc[CInt]()
      !v = value
      check(socket.setsockopt(fd, level, opt, v.asInstanceOf[Ptr[Byte]], sizeof[CInt].toUInt), Syscall.SetOption)
    }

  private def ipv4(host: String, port: Int)(using Zone): Either[NetError, Ptr[sockaddr_in]] =
    val ip  = if host == "0.0.0.0" || host == "*" then "0.0.0.0" else host
    val raw = inet_addr(toCString(ip))
    if raw == 0xffffffff.toUInt && ip != "255.255.255.255" then Left(NetError.NotAnAddress(host))
    else
      val addr = alloc[sockaddr_in]()
      addr.sin_family = socket.AF_INET.toUShort
      addr.sin_port = htons(port.toUShort)
      addr.sin_addr._1 = raw
      Right(addr)
  end ipv4

  private def ntohPort(p: in_port_t): Int =
    val x = p.toInt
    ((x & 0xff) << 8) | ((x >> 8) & 0xff)
end Net
