package heddle.internal.posix

import java.io.IOException
import scala.scalanative.posix.arpa.inet.*
import scala.scalanative.posix.errno.errno
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.netinet.tcp.*
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

  @blocking
  def connect(host: String, port: Int): Int =
    Zone {
      val fd = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
      if fd < 0 then throw io("socket")
      try
        val addr = alloc[sockaddr_in]()
        fill(addr, host, port)
        val rc = socket.connect(fd, addr.asInstanceOf[Ptr[socket.sockaddr]], sizeof[sockaddr_in].toUInt)
        if rc != 0 then throw io(s"connect $host:$port")
        fd
      catch
        case e: Throwable =>
          unistd.close(fd)
          throw e
      end try
    }

  @blocking
  def accept(listenFd: Int): Int =
    Zone {
      val addr    = alloc[sockaddr_in]()
      val addrlen = alloc[socket.socklen_t]()
      !addrlen = sizeof[sockaddr_in].toUInt
      val fd = socket.accept(listenFd, addr.asInstanceOf[Ptr[socket.sockaddr]], addrlen)
      if fd < 0 then throw io("accept")
      fd
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

  @blocking
  def read(fd: Int, dst: Array[Byte], off: Int, len: Int): Int =
    if len <= 0 then 0
    else
      val n = unistd.read(fd, dst.at(off), len.toUSize).toLong
      if n < 0 then
        if errno == scala.scalanative.posix.errno.EINTR then read(fd, dst, off, len)
        else throw io("read")
      else n.toInt

  @blocking
  def write(fd: Int, src: Array[Byte], off: Int, len: Int): Int =
    if len <= 0 then 0
    else
      val n = unistd.write(fd, src.at(off), len.toUSize).toLong
      if n < 0 then
        if errno == scala.scalanative.posix.errno.EINTR then write(fd, src, off, len)
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
