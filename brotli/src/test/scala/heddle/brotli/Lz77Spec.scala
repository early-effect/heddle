package heddle.brotli

import zio.*
import zio.test.*

object Lz77Spec extends ZIOSpecDefault:
  def spec =
    suite("Lz77")(
      test("commands cover the input"):
        val data = ("n" * 64 + "xyz").getBytes
        val cmds = Lz77.compress(data)
        assertTrue(Lz77.covers(data, cmds), cmds.nonEmpty)
      ,
      test("repeated bytes produce a distance-1 copy"):
        val cmds = Lz77.compress(("n" * 32).getBytes)
        val got  = cmds.toList.map(c => (c.insert, c.copy, c.dist))
        assertTrue(got.exists(t => t._3 == 1 && t._2 >= 4), got.nonEmpty)
      ,
      test("unique bytes are insert-only"):
        val data = (0 until 20).map(_.toByte).toArray
        val cmds = Lz77.compress(data)
        assertTrue(cmds.forall(_.dist == 0), cmds.map(_.insert).sum == data.length)
      ,
      test("commands cover and reconstruct arbitrary input"):
        check(BrotliGens.bytes) { data =>
          val cmds = Lz77.compress(data)
          assertTrue(Lz77.covers(data, cmds), Lz77.reconstruct(cmds).toSeq == data.toSeq)
        }
      ,
      test("copy distance stays inside the WBITS=16 window"):
        val data = Array.fill(65521 + 4)(0.toByte)
        data(0) = 'Q'; data(1) = 'R'; data(2) = 'S'; data(3) = 'T'
        data(65521) = 'Q'; data(65522) = 'R'; data(65523) = 'S'; data(65524) = 'T'
        val cmds = Lz77.compress(data)
        assertTrue(cmds.forall(c => c.dist == 0 || (c.dist >= 1 && c.dist <= Lz77.MaxDistance))),
    ) @@ TestAspect.timeout(30.seconds)
end Lz77Spec
