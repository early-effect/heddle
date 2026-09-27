package heddle

import BytesLength.*
import heddle.server.{Http2Config, OutOfRange, Setting}
import zio.*
import zio.test.*

object ServerConfigSpec extends ZIOSpecDefault:
  private val anyLong =
    Gen.oneOf(Gen.long, Gen.elements(Long.MinValue, -1L, 0L, 1L, Int.MaxValue.toLong + 1, Long.MaxValue))

  private val frameSizes = Gen.oneOf(Gen.long(0, 20_000_000), Gen.elements(16_383L, 16_384L, 16_777_215L, 16_777_216L))

  def spec = suite("Server.Config")(
    test("the defaults are valid"):
      assertTrue(Server.Config.default.validate == Right(Server.Config.default))
    ,
    test("validate names exactly the settings out of range, every one of them"):
      check(Gen.int, anyLong, anyLong, frameSizes, Gen.int(-2, 2)) { (port, chunk, header, frame, outstanding) =>
        val config = Server.Config.default.copy(
          port = port,
          chunkSize = BytesLength(chunk),
          maxHeaderBytes = BytesLength(header),
          http2Config = Http2Config(maxFrameSize = BytesLength(frame), maxOutstandingFrames = outstanding),
        )
        val expected = Set(
          Option.when(port < 0 || port > 65535)(Setting.Port),
          Option.when(chunk < 1 || chunk > Int.MaxValue)(Setting.ChunkSize),
          Option.when(header < 1 || header > Int.MaxValue)(Setting.MaxHeaderBytes),
          Option.when(frame < 16_384 || frame > 16_777_215)(Setting.MaxFrameSize),
          Option.when(outstanding < 1)(Setting.MaxOutstandingFrames),
        ).flatten
        val reported = config.validate.fold(_.map(_.setting).toSet, _ => Set.empty[Setting])
        assertTrue(reported == expected)
      }
    ,
    test("loading config refuses what validate refuses"):
      val provider = ConfigProvider.fromMap(Map("heddle.server.http2.maxFrameSize" -> "100"))
      ZIO.config(Server.Config.descriptor).withConfigProvider(provider).flip.map { err =>
        assertTrue(err.toString.contains(OutOfRange(Setting.MaxFrameSize, 100, 16_384, 16_777_215).message))
      }
    ,
    test("a size that does not fit an Int saturates instead of wrapping"):
      check(anyLong) { n =>
        val expected = if n > Int.MaxValue then Int.MaxValue else if n < Int.MinValue then Int.MinValue else n.toInt
        assertTrue(BytesLength(n).toInt == expected)
      }
    ,
    test("K and M compose with the checked sizes"):
      assertTrue(Server.Config.default.copy(chunkSize = 2.G).validate.isLeft),
  ) @@ TestAspect.timeout(60.seconds)
end ServerConfigSpec
