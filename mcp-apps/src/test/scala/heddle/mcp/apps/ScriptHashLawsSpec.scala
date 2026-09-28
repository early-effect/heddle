package heddle.mcp.apps

import zio.*
import zio.json.ast.Json
import zio.test.*

object ScriptHashLawsSpec extends ZIOSpecDefault:
  private val base64 = Gen.elements("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toSeq*)

  def spec = suite("ScriptHash")(
    test("a CSP hash source is sha256- and the base64 digest, quoted in script-src"):
      val h = ScriptHash.of("abc")
      assertTrue(
        h.value == "sha256-ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0=",
        h.source == "'sha256-ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0='",
      )
    ,
    test("every computed hash parses back to itself"):
      check(Gen.string)(s => assertTrue(ScriptHash.from(ScriptHash.of(s).value) == Right(ScriptHash.of(s))))
    ,
    test("text that could widen or end script-src is never a hash"):
      val h = ScriptHash.of("x").value
      check(Gen.elements("'unsafe-inline'", "; connect-src *", " 'self'", "'", "\n", ",", "*")) { attack =>
        assertTrue(
          ScriptHash.from(h + attack).isLeft,
          ScriptHash.from(attack + h).isLeft,
          ScriptHash.from(h.dropRight(1) + attack).isLeft,
        )
      }
    ,
    test("only the canonical base64 of 32 bytes is a digest: 43 characters, zero padding bits, one ="):
      check(Gen.listOfN(43)(base64).map(_.mkString)) { digest =>
        val zeroBits = "AEIMQUYcgkosw048".contains(digest.last)
        assertTrue(
          ScriptHash.from(s"sha256-$digest=").isRight == zeroBits,
          ScriptHash.from(s"sha256-$digest").isLeft,
          ScriptHash.from(s"sha384-$digest=") == Left(ScriptHashError.NotSha256),
        )
      }
    ,
    test("a server's hashes are read back; a bad one is reported and dropped, never kept"):
      val good = ScriptHash.of("view")
      val meta = Json.Obj(
        UiMeta.HeddleKey -> Json.Obj(
          "scriptHashes" -> Json.Arr(Json.Str(good.value), Json.Str("'unsafe-inline'"), Json.Num(1))
        )
      )
      assertTrue(
        UiMeta.decodeScripts(Some(UiMeta.encodeScripts(Chunk(good)))) == (Chunk(good), Chunk.empty),
        UiMeta.decodeScripts(Some(meta))._1 == Chunk(good),
        UiMeta.decodeScripts(Some(meta))._2.collect { case MetaProblem.BadScriptHash(raw, _) => raw } ==
          Chunk("'unsafe-inline'"),
        UiMeta.decodeScripts(None) == (Chunk.empty, Chunk.empty),
      ),
  ) @@ TestAspect.timeout(60.seconds)
end ScriptHashLawsSpec
