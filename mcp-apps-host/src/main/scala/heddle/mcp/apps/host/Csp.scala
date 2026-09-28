package heddle.mcp.apps.host

import heddle.mcp.apps.{Origin, ScriptHash}
import heddle.mcp.apps.ui.SandboxGrant
import zio.Chunk
import zio.json.JsonCodec

/** A `Content-Security-Policy` value for one view, compiled from what its host granted. The relay's document carries it
  * (a response header when the relay is served, the first `<meta>` when it is `srcdoc`), and the view inherits it.
  */
final case class Csp private (header: String)

object Csp:
  /** Which inline scripts may run. */
  enum Scripts derives JsonCodec:
    /** The hashes a server declared for its view, beside the relay's own. Nothing else inline runs, so a server whose
      * hashes were all refused runs no script of its own.
      */
    case Hashed(hashes: Chunk[ScriptHash])

    /** A server that declared no hashes gets the spec's `'unsafe-inline'`. Browsers ignore `'unsafe-inline'` next to a
      * hash, so the two never mix.
      */
    case AnyInline
  end Scripts

  object Scripts:
    /** `declared` is `None` only when the server declared no hashes at all. */
    def of(declared: Option[Chunk[ScriptHash]]): Scripts = declared.fold(AnyInline)(Hashed(_))

  /** The spec's restrictive default, narrowed: no `'self'` (the relay's origin is not the view's), `base-uri 'none'`,
    * and only the origins the host granted, per directive.
    */
  def compile(grant: SandboxGrant, scripts: Scripts, relay: ScriptHash): Csp =
    val n                        = grant.network
    def origins(os: Set[Origin]) = os.toList.map(_.render).sorted
    def listed(os: Set[Origin])  = if os.isEmpty then List("'none'") else origins(os)
    val resources                = origins(n.resources)
    val runnable                 = scripts match
      case Scripts.Hashed(hs) => (relay +: hs).distinct.map(_.source).toList
      case Scripts.AnyInline  => List("'unsafe-inline'")
    val directives: List[(String, List[String])] = List(
      "default-src" -> List("'none'"),
      "script-src"  -> (runnable ++ resources),
      "style-src"   -> ("'unsafe-inline'" :: resources),
      "img-src"     -> ("data:" :: resources),
      "media-src"   -> ("data:" :: resources),
    ) ++ Option.when(resources.nonEmpty)("font-src" -> resources) ++ List(
      "connect-src" -> listed(n.connect),
      "frame-src"   -> listed(n.frames),
      "base-uri"    -> listed(n.base),
      "object-src"  -> List("'none'"),
    )
    Csp(directives.map((name, sources) => (name :: sources).mkString(" ")).mkString("; "))
  end compile
end Csp
