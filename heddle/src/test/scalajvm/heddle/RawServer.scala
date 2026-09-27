package heddle

import java.io.InputStream
import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import zio.*

/** A byte-level HTTP peer for client tests: it answers each request head with `reply`, then keeps the socket or closes
  * it. It can send what a real server should not (no framing, a stale keep-alive).
  *
  * java.net `accept` and `read` ignore thread interrupts, so cancellation closes the socket instead.
  */
object RawServer:
  enum After:
    case Keep, Close

  final case class Seen(heads: Ref[Chunk[String]], accepted: Ref[Int])

  def apply(reply: String => String, after: After): ZIO[Scope, Throwable, (String, Seen)] =
    for
      ss <- ZIO.acquireRelease(ZIO.attempt(ServerSocket(0, 50, InetAddress.getLoopbackAddress)))(s =>
        ZIO.succeed(s.close())
      )
      heads    <- Ref.make(Chunk.empty[String])
      accepted <- Ref.make(0)
      _        <- accept(ss, reply, after, heads, accepted).forever.forkScoped
    yield (s"http://127.0.0.1:${ss.getLocalPort}", Seen(heads, accepted))

  private def accept(
      ss: ServerSocket,
      reply: String => String,
      after: After,
      heads: Ref[Chunk[String]],
      accepted: Ref[Int],
  ): Task[Unit] =
    ZIO.attemptBlockingCancelable(ss.accept())(ZIO.succeed(ss.close())).flatMap { sock =>
      accepted.update(_ + 1) *> serve(sock, reply, after, heads).ensuring(ZIO.succeed(sock.close())).fork.unit
    }

  private def serve(sock: Socket, reply: String => String, after: After, heads: Ref[Chunk[String]]): Task[Unit] =
    ZIO.attemptBlockingCancelable(readHead(sock.getInputStream))(ZIO.succeed(sock.close())).flatMap {
      case None       => ZIO.unit
      case Some(head) =>
        heads.update(_ :+ head) *>
          ZIO.attemptBlocking {
            sock.getOutputStream.write(reply(head).getBytes(StandardCharsets.US_ASCII))
            sock.getOutputStream.flush()
          } *> (after match
            case After.Keep  => serve(sock, reply, after, heads)
            case After.Close => ZIO.unit)
    }

  private def readHead(in: InputStream): Option[String] =
    val sb = StringBuilder()
    var b  = in.read()
    while b >= 0 && !sb.endsWith("\r\n\r\n") do
      sb.append(b.toChar)
      if !sb.endsWith("\r\n\r\n") then b = in.read()
    if sb.isEmpty then None else Some(sb.toString)
end RawServer
