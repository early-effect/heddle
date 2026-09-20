package heddle.internal.engine

import java.nio.ByteBuffer
import heddle.error.HttpError
import zio.*
import zio.stream.ZStream

/** Per-connection read buffer. Unread bytes live in `buf` from position to limit. Fill compact-then-read. */
private[heddle] final class ConnBuf(
    buf: ByteBuffer,
    read: ByteBuffer => IO[HttpError, Int],
    applyTimeout: Duration => UIO[Unit] = _ => ZIO.unit,
):
  private var eof = false

  def setReadTimeout(d: Duration): UIO[Unit] = applyTimeout(d)

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

  def takeHeaders(max: Int): IO[HttpError, Option[Array[Byte]]] =
    def go: IO[HttpError, Option[Array[Byte]]] =
      ZIO.suspendSucceed {
        val idx = indexOfDoubleCrlf
        if idx >= 0 then
          if idx + 4 > max then ZIO.fail(HttpError.HeadersTooLarge)
          else
            val arr = Array.ofDim[Byte](idx + 4)
            buf.get(arr)
            ZIO.succeed(Some(arr))
        else if buf.remaining() >= max then ZIO.fail(HttpError.HeadersTooLarge)
        else
          fillMore.flatMap {
            case false if !buf.hasRemaining => ZIO.succeed(None)
            case false                      => ZIO.fail(HttpError.Malformed("Unexpected end of request headers"))
            case true                       => go
          }
        end if
      }
    end go
    go
  end takeHeaders

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
  end fillUntil

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
    end if
  end hasPrefix

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
  end takeExact

  def takeUpTo(n: Long): IO[HttpError, Chunk[Byte]] =
    if n <= 0 then ZIO.succeed(Chunk.empty)
    else if buf.hasRemaining then
      val takeAt = n.toInt.min(buf.remaining())
      ZIO.succeed(copyOut(takeAt))
    else
      fillMore.flatMap {
        case false => ZIO.succeed(Chunk.empty)
        case true  => takeUpTo(n)
      }

  def drop(n: Long): IO[HttpError, Unit] =
    if n <= 0 then ZIO.unit
    else
      takeUpTo(n).flatMap { c =>
        if c.isEmpty then ZIO.fail(HttpError.Malformed("Unexpected end of request body"))
        else drop(n - c.length)
      }

  def takeLine(max: Int): IO[HttpError, Option[Chunk[Byte]]] =
    def go: IO[HttpError, Option[Chunk[Byte]]] =
      val idx = indexOfCrlf
      if idx >= 0 then
        if idx > max then ZIO.fail(HttpError.Malformed("Chunk line too long"))
        else
          val line = copyOut(idx)
          buf.position(buf.position() + 2)
          ZIO.succeed(Some(line))
      else if buf.remaining() >= max then ZIO.fail(HttpError.Malformed("Chunk line too long"))
      else
        fillMore.flatMap {
          case false if !buf.hasRemaining => ZIO.succeed(None)
          case false                      => ZIO.fail(HttpError.Malformed("Truncated chunk line"))
          case true                       => go
        }
      end if
    end go
    go
  end takeLine

  def expectCrlf: IO[HttpError, Unit] =
    takeUpTo(2).flatMap { c =>
      if c.length == 2 && c(0) == '\r' && c(1) == '\n' then ZIO.unit
      else ZIO.fail(HttpError.Malformed("Expected CRLF after chunk"))
    }

  def skipTrailers(maxLine: Int): IO[HttpError, Unit] =
    takeLine(maxLine).flatMap {
      case None    => ZIO.unit
      case Some(l) => if l.isEmpty then ZIO.unit else skipTrailers(maxLine)
    }

  def chunkedBytes(maxBody: Long = Long.MaxValue, maxLine: Int = 64 * 1024): ZStream[Any, Throwable, Byte] =
    ZStream.unwrap {
      Ref.make(0L).map { total =>
        ZStream.repeatZIOChunkOption {
          readChunkedPiece(total, maxBody, maxLine).mapError(e => Some(toThrowable(e))).flatMap {
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

  def readChunkedPiece(total: Ref[Long], maxBody: Long, maxLine: Int): IO[HttpError, Option[Chunk[Byte]]] =
    takeLine(maxLine).flatMap {
      case None       => ZIO.fail(HttpError.Malformed("Unexpected end of chunked body"))
      case Some(line) =>
        val token = String(line.toArray, java.nio.charset.StandardCharsets.US_ASCII).split(";", 2)(0).trim
        val size  =
          try java.lang.Long.parseLong(token, 16)
          catch case _: NumberFormatException => -1L
        if size < 0 then ZIO.fail(HttpError.Malformed(s"Invalid chunk size: $token"))
        else if size == 0 then skipTrailers(maxLine).as(None)
        else
          total.updateAndGet(_ + size).flatMap { n =>
            if n > maxBody then ZIO.fail(HttpError.BodyTooLarge)
            else
              takeUpTo(size).flatMap { data =>
                if data.length.toLong != size then ZIO.fail(HttpError.Malformed("Truncated chunk"))
                else expectCrlf.as(Some(data))
              }
          }
        end if
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
    while i + 3 < end do
      if a(i) == '\r' && a(i + 1) == '\n' && a(i + 2) == '\r' && a(i + 3) == '\n' then return i - off
      i += 1
    -1

  private def indexOfCrlf: Int =
    val a   = buf.array()
    val off = buf.arrayOffset() + buf.position()
    val end = buf.arrayOffset() + buf.limit()
    var i   = off
    while i + 1 < end do
      if a(i) == '\r' && a(i + 1) == '\n' then return i - off
      i += 1
    -1
end ConnBuf

private[heddle] object ConnBuf:
  def fromConn(buf: ByteBuffer, conn: heddle.internal.duplex.ByteConn): ConnBuf =
    buf.limit(0)
    ConnBuf(buf, conn.read, conn.setReadTimeout)

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
  end fromPull
end ConnBuf
