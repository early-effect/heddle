package heddle

import zio.{Chunk, Random}
import zio.test.Gen

/** Byte chunks for property tests. The length shrinks; the bytes come from one `Random.nextBytes` call, since drawing a
  * few kilobytes one `Gen.byte` at a time is slow enough on Node 20 to time a suite out.
  */
object ByteGens:
  def upTo(max: Int): Gen[Any, Chunk[Byte]] =
    Gen.int(0, max).flatMap(n => Gen.fromZIO(Random.nextBytes(n)))

  def nonEmptyUpTo(max: Int): Gen[Any, Chunk[Byte]] =
    Gen.int(1, max).flatMap(n => Gen.fromZIO(Random.nextBytes(n)))
