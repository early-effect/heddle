package heddle.internal.posix

import scala.scalanative.posix.string.strerror
import scala.scalanative.unsafe.fromCString

private[heddle] enum Syscall(val label: String):
  case Socket      extends Syscall("socket")
  case Bind        extends Syscall("bind")
  case Listen      extends Syscall("listen")
  case Connect     extends Syscall("connect")
  case Accept      extends Syscall("accept")
  case Read        extends Syscall("read")
  case Write       extends Syscall("write")
  case Poll        extends Syscall("poll")
  case GetFlags    extends Syscall("fcntl(F_GETFL)")
  case SetFlags    extends Syscall("fcntl(F_SETFL)")
  case SetOption   extends Syscall("setsockopt")
  case SocketError extends Syscall("getsockopt(SO_ERROR)")
  case Pipe        extends Syscall("pipe")
end Syscall

/** A POSIX socket call that failed, with the `errno` it left. */
private[heddle] enum NetError(val message: String) extends FfiError:
  case Failed(call: Syscall, errno: Int)        extends NetError(s"${call.label}: ${NetError.describe(errno)}")
  case Unresolved(host: String, reason: String) extends NetError(s"cannot resolve $host: $reason")
  case NoAddress(host: String)                  extends NetError(s"$host resolved to no address")
  case NotAnAddress(host: String)               extends NetError(s"$host is not an IPv4 address")

private[heddle] object NetError:
  def describe(errno: Int): String = fromCString(strerror(errno))
