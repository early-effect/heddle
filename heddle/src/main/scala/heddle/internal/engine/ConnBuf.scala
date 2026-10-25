package heddle.internal.engine

import java.nio.ByteBuffer
import heddle.internal.engine.ConnBuf.ChunkedState
import heddle.error.{HttpError, WireError}
import heddle.internal.Ascii
import zio.*
import zio.stream.ZStream

/** Per-connection read buffer. Unread bytes live in `buf` from position to limit. Fill compact-then-read. */
private[heddle] final class ConnBuf(
    buf: ByteBuffer,
    read: ByteBuffer => IO[HttpError, Int],
    applyTimeout: Duration => UIO[Unit] = _ => ZIO.unit,
    /** `setReadTimeout` armed `SO_TIMEOUT`. The socket unblocks the read, so the keep-alive loop does not also race a
      * ZIO timeout.
      */
    val socketDeadline: Boolean = false,
):
  private var eof = false

  /** `nanoTime` deadline for this header block. `0` until a header read is about to block. */
  private var headerDeadline = 0L

  def setReadTimeout(d: Duration): UIO[Unit] = applyTimeout(d)

  def clearHeaderBudget(): Unit =
    headerDeadline = 0L

  /** The preface wait timed out. The next header read fails instead of starting a second window. */
  def expireHeaderBudget(): Unit =
    headerDeadline = java.lang.System.nanoTime()

  private def headerBudgetSpent: Boolean =
    val deadline = headerDeadline
    deadline != 0L && deadline <= java.lang.System.nanoTime()

  def prefaceLook(preface: Array[Byte], maxHeader: Int): PrefaceLook =
    PrefaceLook.classify(
      buf.array(),
      buf.arrayOffset() + buf.position(),
      buf.remaining(),
      preface,
      maxHeader,
    )

  /** One more header byte. On a live socket the first blocking read stamps `timeout`, and later bytes do not move it.
    */
  def fillHeader(timeout: Duration): IO[HttpError, Boolean] =
    ZIO.suspendSucceed {
      if !socketDeadline then fillMore
      else
        val now = java.lang.System.nanoTime()
        if headerDeadline == 0L then headerDeadline = now + timeout.toNanos
        val left = headerDeadline - now
        if left <= 0L then ZIO.fail(HttpError.Timeout)
        else
          setReadTimeout(Duration.fromNanos(left)) *>
            fillMore.catchSome {
              case HttpError.Io(e) if IoKind.socketTimeout(e) || IoKind.closedByInterrupt(e) =>
                headerDeadline = java.lang.System.nanoTime()
                ZIO.fail(HttpError.Timeout)
            }
    }

  /** One more byte toward a preface or header.
    *
    * An empty buffer keeps the socket timeout already installed and does not stamp the budget, so a request that
    * arrives in that read never calls `nanoTime`. Bytes already buffered stamp the budget, and later bytes do not move
    * it.
    */
  def fillPreface(timeout: Duration): IO[HttpError, Boolean] =
    ZIO.suspendSucceed {
      val waiting =
        if socketDeadline && buf.hasRemaining then fillHeader(timeout)
        else fillMore
      waiting.catchSome {
        case HttpError.Io(e) if IoKind.socketTimeout(e) || IoKind.closedByInterrupt(e) =>
          expireHeaderBudget()
          ZIO.fail(HttpError.Timeout)
      }
    }

  def unread(extra: Chunk[Byte]): Unit =
    if extra.isEmpty then ()
    else
      val rem  = buf.remaining()
      val need = extra.length + rem
      val arr  = Array.ofDim[Byte](need)
      extra.copyToArray(arr, 0)
      if rem > 0 then buf.get(arr, extra.length, rem)
      buf.clear()
      if need > buf.capacity then
        // cannot grow this buffer; remaining extra stays unreadable. Tests use 8k+ buffers.
        val n = need.min(buf.capacity)
        buf.put(arr, 0, n)
      else buf.put(arr, 0, need)
      buf.flip()

  def takeHeaders(max: Int, headerTimeout: Duration = Duration.Zero): IO[HttpError, Option[Array[Byte]]] =
    def go: IO[HttpError, Option[Array[Byte]]] =
      ZIO.suspendSucceed {
        if headerBudgetSpent then ZIO.fail(HttpError.Timeout)
        else
          val idx = indexOfDoubleCrlf
          if idx >= 0 then
            if idx + 4 > max then ZIO.fail(HttpError.HeadersTooLarge)
            else
              clearHeaderBudget()
              val arr = Array.ofDim[Byte](idx + 4)
              buf.get(arr)
              ZIO.succeed(Some(arr))
          else if buf.remaining() >= max then ZIO.fail(HttpError.HeadersTooLarge)
          else
            nextHeaderByte(headerTimeout).flatMap {
              case false if !buf.hasRemaining =>
                clearHeaderBudget()
                ZIO.succeed(None)
              case false => ZIO.fail(HttpError.Malformed(WireError.TruncatedHeaders))
              case true  => go
            }
      }
    go

  private def nextHeaderByte(headerTimeout: Duration): IO[HttpError, Boolean] =
    if socketDeadline && headerTimeout.toNanos > 0L then fillHeader(headerTimeout)
    else fillMore

  /** Block until `n` bytes are buffered or EOF. Does not consume. */
  def fillUntil(n: Int): IO[HttpError, Int] =
    def go: IO[HttpError, Int] =
      ZIO.suspendSucceed {
        val rem = buf.remaining()
        if n <= 0 || rem >= n || eof then ZIO.succeed(rem)
        else
          fillMore.flatMap {
            case false => ZIO.succeed(buf.remaining())
            case true  => go
          }
      }
    go

  def hasPrefix(bytes: Array[Byte]): Boolean =
    val n = bytes.length
    if buf.remaining() < n then false
    else
      val a   = buf.array()
      val off = buf.arrayOffset() + buf.position()
      var i   = 0
      var eq  = true
      while i < n && eq do
        eq = a(off + i) == bytes(i)
        i += 1
      eq

  def peek(n: Int): Chunk[Byte] =
    val take = n.min(buf.remaining()).max(0)
    if take == 0 then Chunk.empty
    else
      val a   = buf.array()
      val off = buf.arrayOffset() + buf.position()
      Chunk.fromArray(java.util.Arrays.copyOfRange(a, off, off + take))

  def takeExact(n: Int): IO[HttpError, Chunk[Byte]] =
    def go(acc: Chunk[Byte]): IO[HttpError, Chunk[Byte]] =
      if acc.length >= n then ZIO.succeed(acc.take(n))
      else if buf.hasRemaining then
        val takeAt = (n - acc.length).min(buf.remaining())
        go(acc ++ copyOut(takeAt))
      else
        fillMore.flatMap {
          case false => ZIO.succeed(acc)
          case true  => go(acc)
        }
    if n <= 0 then ZIO.succeed(Chunk.empty) else go(Chunk.empty)

  def takeUpTo(n: Long): IO[HttpError, Chunk[Byte]] =
    ZIO.suspendSucceed {
      if n <= 0 then ZIO.succeed(Chunk.empty)
      else if buf.hasRemaining then
        val takeAt = n.min(buf.remaining().toLong).toInt
        ZIO.succeed(copyOut(takeAt))
      else
        fillMore.flatMap {
          case false => ZIO.succeed(Chunk.empty)
          case true  => takeUpTo(n)
        }
    }

  def drop(n: Long): IO[HttpError, Unit] =
    if n <= 0 then ZIO.unit
    else
      takeUpTo(n).flatMap { c =>
        if c.isEmpty then ZIO.fail(HttpError.Malformed(WireError.TruncatedBody))
        else drop(n - c.length)
      }

  def takeLine(max: Int): IO[HttpError, Option[Chunk[Byte]]] =
    def go(acc: Chunk[Byte]): IO[HttpError, Option[Chunk[Byte]]] =
      ZIO.suspendSucceed {
        if acc.lastOption.contains('\r'.toByte) && buf.hasRemaining && buf.get(buf.position()) == '\n' then
          buf.position(buf.position() + 1)
          ZIO.succeed(Some(acc.dropRight(1)))
        else
          val idx = indexOfCrlf
          if idx >= 0 then
            if acc.length.toLong + idx > max.toLong then ZIO.fail(HttpError.Malformed(WireError.ChunkLineTooLong))
            else
              val line = acc ++ copyOut(idx)
              buf.position(buf.position() + 2)
              ZIO.succeed(Some(line))
          else
            // A trailing CR may be the first delimiter byte, not part of the bounded line.
            val trailingCr =
              if buf.hasRemaining then buf.get(buf.limit() - 1) == '\r'
              else acc.lastOption.contains('\r'.toByte)
            val length = acc.length.toLong + buf.remaining() - (if trailingCr then 1L else 0L)
            if length > max.toLong then ZIO.fail(HttpError.Malformed(WireError.ChunkLineTooLong))
            else
              val next = acc ++ copyOut(buf.remaining())
              fillMore.flatMap {
                case false if next.isEmpty => ZIO.succeed(None)
                case false                 => ZIO.fail(HttpError.Malformed(WireError.TruncatedChunk))
                case true                  => go(next)
              }
      }
    go(Chunk.empty)

  def expectCrlf: IO[HttpError, Unit] =
    ZIO.suspendSucceed {
      takeExact(2).flatMap { c =>
        if c.length == 2 && c(0) == '\r' && c(1) == '\n' then ZIO.unit
        else ZIO.fail(HttpError.Malformed(WireError.MissingChunkCrlf))
      }
    }

  def skipTrailers(maxLine: Int): IO[HttpError, Unit] =
    takeLine(maxLine).flatMap {
      case None    => ZIO.fail(HttpError.Malformed(WireError.TruncatedChunk))
      case Some(l) => if l.isEmpty then ZIO.unit else skipTrailers(maxLine)
    }

  def chunkedBytes(maxBody: Long, maxLine: Int): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap {
      Ref.make(ChunkedState.initial).map { state =>
        ZStream.repeatZIOChunkOption {
          readChunkedPiece(state, maxBody, maxLine).mapError(e => Some(toThrowable(e))).flatMap {
            case None    => ZIO.fail(None)
            case Some(c) => ZIO.succeed(c)
          }
        }
      }
    }

  def takeBytes(left: Ref[Long], chunkSize: Int): ZStream[Any, Throwable, Byte] =
    ZStream.repeatZIOChunkOption {
      left.get.flatMap { rem =>
        if rem <= 0 then ZIO.fail(None)
        else
          takeUpTo(rem.min(chunkSize.toLong)).mapError(e => Some(toThrowable(e))).flatMap { c =>
            if c.isEmpty then ZIO.fail(Some(RuntimeException("Unexpected end of request body")))
            else left.update(_ - c.length).as(c)
          }
      }
    }

  def readChunkedPiece(
      state: Ref[ChunkedState],
      maxBody: Long,
      maxLine: Int,
  ): IO[HttpError, Option[Chunk[Byte]]] =
    state.get.flatMap {
      case ChunkedState.Done             => ZIO.succeed(None)
      case ChunkedState.Delimiter(total) =>
        expectCrlf *> state.set(ChunkedState.Size(total)) *> readChunkedPiece(state, maxBody, maxLine)
      case ChunkedState.Payload(total, remaining) =>
        takeUpTo(remaining).flatMap { data =>
          if data.isEmpty then ZIO.fail(HttpError.Malformed(WireError.TruncatedChunk))
          else
            val left = remaining - data.length
            val next = if left == 0 then ChunkedState.Delimiter(total) else ChunkedState.Payload(total, left)
            state.set(next).as(Some(data))
        }
      case ChunkedState.Size(total) =>
        takeLine(maxLine).flatMap {
          case None       => ZIO.fail(HttpError.Malformed(WireError.TruncatedChunk))
          case Some(line) =>
            val token = String(line.toArray, java.nio.charset.StandardCharsets.US_ASCII).takeWhile(_ != ';').trim
            Ascii.hex(token) match
              case None       => ZIO.fail(HttpError.Malformed(WireError.BadChunkSize(token)))
              case Some(0)    => skipTrailers(maxLine) *> state.set(ChunkedState.Done).as(None)
              case Some(size) =>
                if size > maxBody - total then ZIO.fail(HttpError.BodyTooLarge)
                else state.set(ChunkedState.Payload(total + size, size)) *> readChunkedPiece(state, maxBody, maxLine)
        }
    }

  def toThrowable(err: HttpError): Throwable =
    err match
      case HttpError.Io(cause) => cause
      case other               => RuntimeException(other.message)

  private def fillMore: IO[HttpError, Boolean] =
    ZIO.suspendSucceed {
      if eof then ZIO.succeed(false)
      else
        if buf.hasRemaining then buf.compact() else buf.clear()
        read(buf).map { n =>
          if n < 0 then
            eof = true
            buf.flip()
            false
          else
            buf.flip()
            true
        }
    }

  private def copyOut(n: Int): Chunk[Byte] =
    val arr = Array.ofDim[Byte](n)
    buf.get(arr)
    Chunk.fromArray(arr)

  private def indexOfDoubleCrlf: Int =
    val a   = buf.array()
    val off = buf.arrayOffset() + buf.position()
    val end = buf.arrayOffset() + buf.limit()
    var i   = off
    while i + 3 < end && !(a(i) == '\r' && a(i + 1) == '\n' && a(i + 2) == '\r' && a(i + 3) == '\n') do i += 1
    if i + 3 < end then i - off else -1

  private def indexOfCrlf: Int =
    val a   = buf.array()
    val off = buf.arrayOffset() + buf.position()
    val end = buf.arrayOffset() + buf.limit()
    var i   = off
    while i + 1 < end && !(a(i) == '\r' && a(i + 1) == '\n') do i += 1
    if i + 1 < end then i - off else -1
end ConnBuf

private[heddle] object ConnBuf:
  enum ChunkedState:
    case Size(total: Long)
    case Payload(total: Long, remaining: Long)
    case Delimiter(total: Long)
    case Done

  object ChunkedState:
    val initial: ChunkedState = Size(0L)

  def fromConn(
      buf: ByteBuffer,
      conn: heddle.internal.duplex.ByteConn,
      socketDeadline: Boolean = false,
  ): ConnBuf =
    buf.limit(0)
    val read = if socketDeadline then conn.readOnCarrier else conn.read
    ConnBuf(buf, read, conn.setReadTimeout, socketDeadline)

  def fromPull(
      buf: ByteBuffer,
      pull: IO[HttpError, Option[Chunk[Byte]]],
      applyTimeout: Duration => UIO[Unit] = _ => ZIO.unit,
  ): ConnBuf =
    buf.limit(0)
    var extra: Chunk[Byte]                         = Chunk.empty
    def put(dest: ByteBuffer, c: Chunk[Byte]): Int =
      val n = c.length.min(dest.remaining())
      var i = 0
      while i < n do
        dest.put(c(i))
        i += 1
      n
    def fill(dest: ByteBuffer): IO[HttpError, Int] =
      if extra.nonEmpty then
        val n = put(dest, extra)
        extra = extra.drop(n)
        ZIO.succeed(n)
      else
        pull.flatMap {
          case None    => ZIO.succeed(-1)
          case Some(c) =>
            val n = put(dest, c)
            if n < c.length then extra = c.drop(n)
            ZIO.succeed(n)
        }
    ConnBuf(buf, fill, applyTimeout)
end ConnBuf
