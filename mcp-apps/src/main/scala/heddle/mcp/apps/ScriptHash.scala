package heddle.mcp.apps

import heddle.error.HeddleError
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.quoted.*
import zio.Chunk
import zio.json.JsonCodec

/** Why text is not a script hash a host will put in `script-src`. */
enum ScriptHashError(val message: String) extends HeddleError:
  case NotSha256                 extends ScriptHashError("a script hash is sha256-<base64 digest>")
  case BadDigest(digest: String) extends ScriptHashError(s"'$digest' is not the canonical base64 of a 32-byte digest")

/** The SHA-256 of one inline script, as CSP names it: `sha256-<base64>`. A host copies these from a server into
  * `script-src`, so `from` accepts exactly the canonical form, and nothing a server sends can add a keyword such as
  * `'unsafe-inline'` or end the directive.
  */
opaque type ScriptHash = String

object ScriptHash:
  private val Prefix   = "sha256-"
  private val Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def of(script: String): ScriptHash =
    Prefix + Base64.getEncoder.encodeToString(
      Sha256.digest(Chunk.fromArray(script.getBytes(StandardCharsets.UTF_8))).toArray
    )

  /** 32 bytes are 43 base64 characters and one `=`. The 43rd carries two padding bits, which must be zero. */
  def from(raw: String): Either[ScriptHashError, ScriptHash] =
    if !raw.startsWith(Prefix) then Left(ScriptHashError.NotSha256)
    else
      val digest    = raw.drop(Prefix.length)
      val canonical = digest.length == 44 && digest.last == '=' &&
        digest.take(43).forall(Alphabet.contains(_)) && Alphabet.indexOf(digest(42)) % 4 == 0
      if canonical then Right(raw) else Left(ScriptHashError.BadDigest(digest))

  /** A literal (`ScriptHash("sha256-…")`), checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ScriptHash = ${ literal('raw) }

  private def literal(raw: Expr[String])(using Quotes): Expr[ScriptHash] =
    import quotes.reflect.report
    raw.value.map(s => (s, from(s))) match
      case None                => report.errorAndAbort("ScriptHash(...) takes a literal; use ScriptHash.from")
      case Some((_, Left(e)))  => report.errorAndAbort(s"not a script hash: ${e.message}")
      case Some((s, Right(_))) => Expr(s)

  /** A JSON string, decoded strictly. */
  given JsonCodec[ScriptHash] = JsonCodec.string.transformOrFail(from(_).left.map(_.message), _.value)

  extension (h: ScriptHash)
    /** `sha256-<base64>`, as a server lists it. */
    def value: String = h

    /** `'sha256-<base64>'`, as it goes in `script-src`. */
    def source: String = s"'$h'"
end ScriptHash
