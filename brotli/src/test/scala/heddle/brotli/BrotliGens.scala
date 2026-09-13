package heddle.brotli

import zio.test.Gen

private[brotli] object BrotliGens:
  val bytes: Gen[Any, Array[Byte]] =
    Gen.oneOf(
      Gen.const(Array.empty[Byte]),
      Gen.int(1, 64).map(n => Array.fill(n)(0.toByte)),
      Gen.int(1, 512).map(n => Array.fill(n)('n'.toByte)),
      Gen.chunkOfBounded(0, 1024)(Gen.byte).map(_.toArray),
    )

  val compressible: Gen[Any, Array[Byte]] =
    Gen.int(256, 2048).map(n => Array.fill(n)('n'.toByte))
end BrotliGens
