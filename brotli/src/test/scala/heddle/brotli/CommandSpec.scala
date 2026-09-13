package heddle.brotli

import zio.*
import zio.test.*

object CommandSpec extends ZIOSpecDefault:
  def spec =
    suite("Command")(
      test("insert length is the inverse of insert"):
        check(Gen.int(0, 8000)) { n =>
          val ins = Command.insert(n)
          assertTrue(Command.insertLength(ins) == n)
        }
      ,
      test("copy length is the inverse of copy"):
        check(Gen.int(2, 8000)) { n =>
          val cpy = Command.copy(n)
          assertTrue(Command.copyLength(cpy) == n)
        }
      ,
      test("command id encode/decode is an inverse"):
        check(Gen.int(0, 400), Gen.int(2, 400), Gen.boolean) { (ins, cpy, dist) =>
          val enc          = Command.encode(ins, cpy, dist)
          val (ic, cc, hd) = Command.decodeId(enc.id)
          val distOk       =
            if dist then hd
            else if enc.insert.code < 8 && enc.copy.code < 16 then !hd
            else true
          assertTrue(ic == enc.insert.code, cc == enc.copy.code, distOk)
        }
      ,
      test("command id splits with org.brotli.dec range LUTs"):
        val cases = for
          ins  <- List(0, 1, 4, 7, 8, 15, 16, 100, 2048)
          cpy  <- List(2, 4, 9, 16, 22, 70, 2047)
          dist <- List(false, true)
        yield (ins, cpy, dist)
        val bad = cases.flatMap { (ins, cpy, dist) =>
          val enc          = Command.encode(ins, cpy, dist)
          val (ic, cc, hd) = Command.decodeId(enc.id)
          val codesOk      = ic == enc.insert.code && cc == enc.copy.code
          val distOk       =
            if dist then hd
            else if enc.insert.code < 8 && enc.copy.code < 16 then !hd
            else true
          if codesOk && distOk then Nil else List((ins, cpy, dist, enc.id, ic, cc, hd))
        }
        assertTrue(bad.isEmpty)
      ,
      test("insert 1 copy 2047 with distance is id 398"):
        val enc = Command.encode(1, 2047, emitDistance = true)
        assertTrue(enc.id == 398, enc.insert.code == 1, enc.copy.code == 22, enc.copy.extraBits == 10),
    ) @@ TestAspect.timeout(30.seconds)
end CommandSpec
