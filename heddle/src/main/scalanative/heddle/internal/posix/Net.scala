package heddle.internal.posix

import java.io.IOException
import scala.scalanative.posix.arpa.inet.*
import scala.scalanative.posix.errno.{EAGAIN, EINPROGRESS, EINTR, EWOULDBLOCK, errno}
import scala.scalanative.posix.fcntl
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.netinet.tcp.*
import scala.scalanative.posix.poll
import scala.scalanative.posix.pollOps.*
import scala.scalanative.posix.string.strerror
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.time.*
import scala.scalanative.posix.sys.timeOps.*
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** POSIX sockets. `Ptr` stays in this package. */
private[heddle] object Net:
  def listen(host: String, port: Int, backlog: Int, reuse: Boolean): Int =
    Zone {
      val fd = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
      if fd < 0 then throw io("socket")
      try
        if reuse then setInt(fd, socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        setNonBlocking(fd)
        val addr = alloc[sockaddr_in]()
        fill(addr, host, port)
        val rc = socket.bind(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], sizeof[sockaddr_in].toUInt)
        if rc != 0 then throw io(s"bind $host:$port")
        if socket.listen(fd, backlog) != 0 then throw io("listen")
        fd
      catch
        case e: Throwable =>
          unistd.close(fd)
          throw e
      end try
    }

  def connect(host: String, port: Int): Int =
    Zone {
      val fd = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
      if fd < 0 then throw io("socket")
      try
        setNonBlocking(fd)
        val addr = alloc[sockaddr_in]()
        fill(addr, host, port)
        val rc = socket.connect(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], sizeof[sockaddr_in].toUInt)
        if rc != 0 && !inProgress then throw io(s"connect $host:$port")
        fd
      catch
        case e: Throwable =>
          unistd.close(fd)
          throw e
      end try
    }

  def connectInProgress: Boolean = inProgress

  def socketError(fd: Int): Int =
    Zone {
      val v   = alloc[CInt]()
      val len = alloc[socket.socklen_t]()
      !v = 0
      !len = sizeof[CInt].toUInt
      val rc = socket.getsockopt(fd, socket.SOL_SOCKET, socket.SO_ERROR, v.asInstanceOf[Ptr[Byte]], len)
      if rc != 0 then throw io("SO_ERROR")
      !v
    }

  def accept(listenFd: Int): Int =
    Zone {
      val addr    = alloc[sockaddr_in]()
      val addrlen = alloc[socket.socklen_t]()
      !addrlen = sizeof[sockaddr_in].toUInt
      val fd = socket.accept(listenFd, addr.asInstanceOf[Ptr[socket.sockaddr]], addrlen)
      if fd < 0 then
        if wouldBlock then -1
        else throw io("accept")
      else
        setNonBlocking(fd)
        fd
    }

  def wouldBlock: Boolean =
    errno == EAGAIN || errno == EWOULDBLOCK

  private def inProgress: Boolean =
    errno == EINPROGRESS || errno == EAGAIN || errno == EWOULDBLOCK

  /** Non-blocking. `timeoutMs = 0` does not occupy a blocking thread. */
  def pollIn(fd: Int, timeoutMs: Int): Boolean =
    Zone {
      val pfd = alloc[poll.struct_pollfd]()
      pfd.fd = fd
      pfd.events = poll.POLLIN.toShort
      pfd.revents = 0.toShort
      val n = poll.poll(pfd, 1.toUInt, timeoutMs)
      if n < 0 then
        if errno == EINTR then pollIn(fd, timeoutMs)
        else throw io("poll")
      else n > 0
    }

  def pollOut(fd: Int, timeoutMs: Int): Boolean =
    Zone {
      val pfd = alloc[poll.struct_pollfd]()
      pfd.fd = fd
      pfd.events = poll.POLLOUT.toShort
      pfd.revents = 0.toShort
      val n = poll.poll(pfd, 1.toUInt, timeoutMs)
      if n < 0 then
        if errno == EINTR then pollOut(fd, timeoutMs)
        else throw io("poll")
      else n > 0
    }

  /** Block in `poll` until any of `fds` is ready. Returns the ready fds. */
  def pollReady(fds: Array[Int], timeoutMs: Int): Array[Int] =
    if fds.isEmpty then Array.empty
    else
      Zone {
        val pfds = alloc[poll.struct_pollfd](fds.length)
        var i    = 0
        while i < fds.length do
          val p = pfds + i
          p.fd = fds(i)
          p.events = (poll.POLLIN | poll.POLLOUT).toShort
          p.revents = 0.toShort
          i += 1
        val n = poll.poll(pfds, fds.length.toUInt, timeoutMs)
        if n < 0 then
          if errno == EINTR then pollReady(fds, timeoutMs)
          else throw io("poll")
        else
          val out = scala.collection.mutable.ArrayBuffer.empty[Int]
          i = 0
          while i < fds.length do
            if (pfds + i).revents.toInt != 0 then out += fds(i)
            i += 1
          out.toArray
        end if
      }

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

  def read(fd: Int, dst: Array[Byte], off: Int, len: Int): Int =
    if len <= 0 then 0
    else
      val n = unistd.read(fd, dst.at(off), len.toUSize).toLong
      if n < 0 then
        if errno == EINTR then read(fd, dst, off, len)
        else if wouldBlock then -2
        else throw io("read")
      else n.toInt

  def write(fd: Int, src: Array[Byte], off: Int, len: Int): Int =
    if len <= 0 then 0
    else
      val n = unistd.write(fd, src.at(off), len.toUSize).toLong
      if n < 0 then
        if errno == EINTR then write(fd, src, off, len)
        else if wouldBlock then -2
        else throw io("write")
      else n.toInt

  def setRecvTimeout(fd: Int, millis: Int): Unit =
    Zone {
      val tv = alloc[timeval]()
      if millis <= 0 then
        tv.tv_sec = 0.toSize
        tv.tv_usec = 0.toSize
      else
        tv.tv_sec = (millis / 1000).toSize
        tv.tv_usec = ((millis % 1000) * 1000).toSize
      val rc =
        socket.setsockopt(
          fd,
          socket.SOL_SOCKET,
          socket.SO_RCVTIMEO,
          tv.asInstanceOf[Ptr[Byte]],
          sizeof[timeval].toUInt,
        )
      if rc != 0 then throw io("SO_RCVTIMEO")
    }

  def setTcpNoDelay(fd: Int, on: Boolean): Unit =
    setInt(fd, IPPROTO_TCP, TCP_NODELAY, if on then 1 else 0)

  def setKeepAlive(fd: Int, on: Boolean): Unit =
    setInt(fd, socket.SOL_SOCKET, socket.SO_KEEPALIVE, if on then 1 else 0)

  def setNonBlocking(fd: Int): Unit =
    val flags = fcntl.fcntl(fd, fcntl.F_GETFL, 0)
    if flags < 0 then throw io("F_GETFL")
    if fcntl.fcntl(fd, fcntl.F_SETFL, flags | fcntl.O_NONBLOCK) < 0 then throw io("O_NONBLOCK")

  private def setInt(fd: Int, level: CInt, opt: CInt, value: Int): Unit =
    Zone {
      val v = alloc[CInt]()
      !v = value
      val rc = socket.setsockopt(fd, level, opt, v.asInstanceOf[Ptr[Byte]], sizeof[CInt].toUInt)
      if rc != 0 then throw io(s"setsockopt $opt")
    }

  private def fill(addr: Ptr[sockaddr_in], host: String, port: Int)(using Zone): Unit =
    addr.sin_family = socket.AF_INET.toUShort
    addr.sin_port = htons(port.toUShort)
    val ip  = if host == "0.0.0.0" || host == "*" then "0.0.0.0" else host
    val raw = inet_addr(toCString(ip))
    if raw == 0xffffffff.toUInt && ip != "255.255.255.255" then throw IOException(s"bad bind host: $host")
    addr.sin_addr._1 = raw

  private def ntohPort(p: in_port_t): Int =
    val x = p.toInt
    ((x & 0xff) << 8) | ((x >> 8) & 0xff)

  private def io(op: String): IOException =
    val msg = fromCString(strerror(errno))
    IOException(s"$op: $msg")
end Net
