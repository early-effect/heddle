package heddle

import heddle.error.WireError
import heddle.internal.h2.{FrameCodec, H2Frame}
import zio.*
import zio.test.*

object FrameCodecLawsSpec extends ZIOSpecDefault:
  private val Max    = H2Frame.DefaultMaxFrameSize
  private val id     = Gen.int(0, Int.MaxValue)
  private val u31    = Gen.int(0, Int.MaxValue)
  private val bytes  = ByteGens.upTo(48)
  private val weight = Gen.int(1, 256)

  private val frame: Gen[Any, H2Frame] = Gen.oneOf(
    (id <*> bytes <*> Gen.boolean <*> Gen.int(0, 255)).map(H2Frame.Data(_, _, _, _)),
    (id <*> bytes <*> Gen.boolean <*> Gen.boolean <*> Gen.boolean <*> u31 <*> weight)
      .map(H2Frame.Headers(_, _, _, _, _, _, _)),
    (id <*> Gen.int).map(H2Frame.RstStream(_, _)),
    Gen.chunkOfBounded(0, 6)(Gen.int(0, 65535) <*> Gen.int).map(H2Frame.Settings(_)),
    Gen.const(H2Frame.SettingsAck),
    Gen.long.map(H2Frame.Ping(_)),
    Gen.long.map(H2Frame.PingAck(_)),
    (u31 <*> Gen.int <*> bytes).map(H2Frame.GoAway(_, _, _)),
    (id <*> u31).map(H2Frame.WindowUpdate(_, _)),
    (id <*> bytes <*> Gen.boolean).map(H2Frame.Continuation(_, _, _)),
    (id <*> Gen.boolean <*> u31 <*> weight).map(H2Frame.Priority(_, _, _, _)),
    (id <*> u31 <*> bytes <*> Gen.boolean).map(H2Frame.PushPromise(_, _, _, _)),
    (Gen.int(10, 255) <*> Gen.int(0, 255) <*> id <*> bytes).map(H2Frame.Unknown(_, _, _, _)),
  )

  /** A well-formed frame header over any type, flags, stream id (reserved bit included), and payload. */
  private val framed: Gen[Any, Chunk[Byte]] =
    (Gen.int(0, 12) <*> Gen.int(0, 255) <*> Gen.int <*> bytes).map { (tpe, flags, streamId, payload) =>
      val n = payload.length
      Chunk[Byte](0, (n >> 8).toByte, n.toByte, tpe.toByte, flags.toByte) ++
        Chunk((streamId >>> 24).toByte, (streamId >>> 16).toByte, (streamId >>> 8).toByte, streamId.toByte) ++ payload
    }

  private val anyBytes = Gen.oneOf(framed, ByteGens.upTo(64))

  def spec = suite("HTTP/2 frame codec laws")(
    test("decode inverts encode and hands back what follows the frame"):
      check(frame, bytes)((f, tail) =>
        assertTrue(FrameCodec.decode(FrameCodec.encode(f) ++ tail, Max) == Right(f -> tail))
      )
    ,
    test("decode is total, and whatever it reads re-encodes to the same frame"):
      check(anyBytes) { raw =>
        FrameCodec.decode(raw, Max) match
          case Right((f, rest)) =>
            assertTrue(FrameCodec.decode(FrameCodec.encode(f), Max) == Right(f -> Chunk.empty), raw.endsWith(rest))
          case Left(_) => assertCompletes
      }
    ,
    test("every strict prefix of a frame is truncated"):
      check(frame, Gen.int(0, 1000)) { (f, cut) =>
        val wire = FrameCodec.encode(f)
        assertTrue(FrameCodec.decode(wire.take(cut % wire.length), Max) == Left(WireError.TruncatedFrame))
      }
    ,
    test("a payload over the limit is refused before it is read"):
      check(ByteGens.nonEmptyUpTo(48)) { data =>
        val wire = FrameCodec.encode(H2Frame.Data(1, data, endStream = false))
        assertTrue(
          FrameCodec.decode(wire, data.length - 1) == Left(WireError.FrameTooLarge(data.length, data.length - 1))
        )
      }
    ,
    test("padding as long as the payload is refused"):
      check(bytes) { data =>
        val payload = Chunk.single((data.length + 1).toByte) ++ data
        val n       = payload.length
        val wire    = Chunk[Byte](0, 0, n.toByte, H2Frame.DataType.toByte, 0x08, 0, 0, 0, 1) ++ payload
        assertTrue(FrameCodec.decode(wire, Max) == Left(WireError.BadPadding(data.length + 1, n)))
      }
    ,
    test("fixed-size frames of the wrong size are refused with their type and length"):
      check(Gen.elements(H2Frame.RstStreamType, H2Frame.PingType, H2Frame.WindowUpdateType, H2Frame.PriorityType)) {
        tpe =>
          val wire = Chunk[Byte](0, 0, 3, tpe.toByte, 0, 0, 0, 0, 1, 1, 2, 3)
          assertTrue(FrameCodec.decode(wire, Max) == Left(WireError.FrameSize(tpe, 3)))
      }
    ,
    test("a SETTINGS acknowledgement with parameters is refused"):
      val wire = Chunk[Byte](0, 0, 6, H2Frame.SettingsType.toByte, 0x01, 0, 0, 0, 0, 0, 4, 0, 0, 0, 1)
      assertTrue(FrameCodec.decode(wire, Max) == Left(WireError.FrameSize(H2Frame.SettingsType, 6))),
  ) @@ TestAspect.timeout(60.seconds)
end FrameCodecLawsSpec
