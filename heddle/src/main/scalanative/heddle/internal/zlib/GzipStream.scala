package heddle.internal.zlib

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import zio.*
import zio.stream.ZStream

/** zlib's streaming deflate, bound directly. javalib's `Deflater.deflate(..., SYNC_FLUSH)` hands zlib its flush field
  * instead of the argument (Scala Native 0.5.12), so a sync flush there emits nothing.
  */
@link("z")
@extern
private object Z:
  /** `z_stream`, field for field; Scala Native lays it out, so `uLong` is right on every ABI. */
  type Stream = CStruct14[
    Ptr[Byte],     // next_in
    CUnsignedInt,  // avail_in
    CUnsignedLong, // total_in
    Ptr[Byte],     // next_out
    CUnsignedInt,  // avail_out
    CUnsignedLong, // total_out
    CString,       // msg
    Ptr[Byte],     // state
    Ptr[Byte],     // zalloc
    Ptr[Byte],     // zfree
    Ptr[Byte],     // opaque
    CInt,          // data_type
    CUnsignedLong, // adler
    CUnsignedLong, // reserved
  ]

  def zlibVersion(): CString = extern

  def deflateInit2_(
      strm: Ptr[Stream],
      level: CInt,
      method: CInt,
      windowBits: CInt,
      memLevel: CInt,
      strategy: CInt,
      version: CString,
      streamSize: CInt,
  ): CInt = extern

  def deflate(strm: Ptr[Stream], flush: CInt): CInt = extern
  def deflateEnd(strm: Ptr[Stream]): CInt           = extern
end Z

/** Gzip that emits as input arrives: each chunk ends in a sync flush, so a reader can inflate everything sent so far.
  * The `z_stream` lives in the stream's scope, so ending or interrupting the stream frees it.
  */
private[heddle] object GzipStream:
  private val ZOk            = 0
  private val ZStreamEnd     = 1
  private val ZSyncFlush     = 2
  private val ZFinish        = 4
  private val ZBufError      = -5
  private val ZDeflated      = 8
  private val DefaultLevel   = -1
  private val GzipWindowBits = 31
  private val MemLevel       = 8
  private val OutBuffer      = 16 * 1024

  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] =
    val opened =
      ZIO.acquireRelease(ZIO.suspendSucceed(ZIO.fromEither(open())).mapError(_.exception))(s => ZIO.succeed(close(s)))
    ZStream.scoped(opened).flatMap { strm =>
      def step(input: Chunk[Byte], flush: Int) =
        ZIO.suspendSucceed(ZIO.fromEither(run(strm, input, flush))).mapError(_.exception)
      val body = in.chunks.filter(_.nonEmpty).mapZIO(step(_, ZSyncFlush))
      val end  = ZStream.fromZIO(step(Chunk.empty, ZFinish))
      (body ++ end).flattenChunks
    }
  end stream

  private def open(): Either[ZlibError, Ptr[Z.Stream]] =
    val size = sizeof[Z.Stream]
    val strm = stdlib.calloc(1.toUSize, size).asInstanceOf[Ptr[Z.Stream]]
    if strm == null then Left(ZlibError.OutOfMemory)
    else
      val rc =
        Z.deflateInit2_(strm, DefaultLevel, ZDeflated, GzipWindowBits, MemLevel, 0, Z.zlibVersion(), size.toInt)
      if rc == ZOk then Right(strm)
      else
        stdlib.free(strm.asInstanceOf[Ptr[Byte]])
        Left(ZlibError.Failed("deflateInit2", rc))
  end open

  private def close(strm: Ptr[Z.Stream]): Unit =
    val _ = Z.deflateEnd(strm)
    stdlib.free(strm.asInstanceOf[Ptr[Byte]])

  /** Feeds `input` and drains output until zlib has nothing more for this flush mode. */
  private def run(strm: Ptr[Z.Stream], input: Chunk[Byte], flush: Int): Either[ZlibError, Chunk[Byte]] =
    val src = input.toArray
    val out = new Array[Byte](OutBuffer)
    val acc = Chunk.newBuilder[Byte]
    strm._1 = if src.isEmpty then null else src.at(0)
    strm._2 = src.length.toUInt
    var failed = Option.empty[ZlibError]
    var more   = true
    while more do
      strm._4 = out.at(0)
      strm._5 = OutBuffer.toUInt
      val rc = Z.deflate(strm, flush)
      if rc != ZOk && rc != ZStreamEnd && rc != ZBufError then
        failed = Some(ZlibError.Failed("deflate", rc))
        more = false
      else
        val produced = OutBuffer - strm._5.toInt
        var i        = 0
        while i < produced do
          acc += out(i)
          i += 1
        more = if flush == ZFinish then rc != ZStreamEnd else produced == OutBuffer
      end if
    end while
    failed.toLeft(acc.result())
  end run
end GzipStream

/** A zlib call that returned an error code. */
private[heddle] enum ZlibError(val message: String) extends heddle.internal.posix.FfiError:
  case OutOfMemory                         extends ZlibError("zlib: out of memory")
  case Failed(function: String, code: Int) extends ZlibError(s"zlib: $function failed ($code)")
