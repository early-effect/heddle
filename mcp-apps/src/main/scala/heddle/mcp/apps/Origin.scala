package heddle.mcp.apps

import scala.annotation.publicInBinary
import scala.quoted.*

/** One web origin a view may reach: scheme, host, and a port when it is not the scheme's default. No wildcards, no
  * path, no userinfo: a policy names exactly what it allows.
  */
final case class Origin @publicInBinary private (scheme: Origin.Scheme, host: String, port: Option[Int]):
  def render: String =
    s"${scheme.render}://$host${port.fold("")(p => s":$p")}"

object Origin:
  enum Scheme(val render: String, val defaultPort: Int):
    case Https extends Scheme("https", 443)
    case Http  extends Scheme("http", 80)
    case Wss   extends Scheme("wss", 443)
    case Ws    extends Scheme("ws", 80)

  /** A literal (`Origin("https://api.example.com")`), checked at compile time. A runtime value goes through `from`. */
  inline def apply(inline raw: String): Origin = ${ literal('raw) }

  /** `https://api.example.com`, `http://localhost:8080`. A path, query, or `*` is a `Left`. */
  def from(raw: String): Either[String, Origin] =
    raw.indexOf("://") match
      case -1  => Left(s"not an origin (scheme://host[:port]): $raw")
      case sep =>
        val rest = raw.substring(sep + 3)
        Scheme.values.find(_.render == raw.substring(0, sep).toLowerCase) match
          case None                                             => Left(s"not a web scheme: $raw")
          case Some(_) if rest.exists(c => "/?#@*".contains(c)) => Left(s"not a bare origin: $raw")
          case Some(scheme)                                     =>
            val (host, port) = splitPort(rest)
            port.flatMap(build(scheme, host, _))

  private def build(scheme: Scheme, host: String, port: Option[Int]): Either[String, Origin] =
    val h = host.toLowerCase
    if !validHost(h) then Left(s"not a host: $host")
    else
      port.find(p => p < 1 || p > 65535) match
        case Some(p) => Left(s"not a port: $p")
        case None    => Right(new Origin(scheme, h, port.filterNot(_ == scheme.defaultPort)))

  private def splitPort(rest: String): (String, Either[String, Option[Int]]) =
    val bracket = rest.lastIndexOf(']')
    val colon   = rest.lastIndexOf(':')
    if colon <= bracket then (rest, Right(None))
    else
      val digits = rest.substring(colon + 1)
      val port   = digits.toIntOption.filter(_ => digits.forall(_.isDigit)).toRight(s"not a port: $digits")
      (rest.substring(0, colon), port.map(Some(_)))

  private def validHost(h: String): Boolean =
    if h.startsWith("[") then
      h.endsWith("]") && h.length > 2 && h.drop(1).dropRight(1).forall(c => c.isLetterOrDigit || c == ':')
    else
      h.nonEmpty && h.length <= 253 && h
        .split('.')
        .forall(label =>
          label.nonEmpty && label.length <= 63 && label.forall(c => c < 128 && (c.isLetterOrDigit || c == '-')) &&
            !label.startsWith("-") && !label.endsWith("-")
        )

  private def literal(raw: Expr[String])(using Quotes): Expr[Origin] =
    import quotes.reflect.report
    raw.value.map(from) match
      case None            => report.errorAndAbort("Origin(...) takes a literal; use Origin.from")
      case Some(Left(e))   => report.errorAndAbort(e)
      case Some(Right(ok)) => '{ new Origin(${ schemeExpr(ok.scheme) }, ${ Expr(ok.host) }, ${ Expr(ok.port) }) }

  private def schemeExpr(s: Scheme)(using Quotes): Expr[Scheme] =
    s match
      case Scheme.Https => '{ Scheme.Https }
      case Scheme.Http  => '{ Scheme.Http }
      case Scheme.Wss   => '{ Scheme.Wss }
      case Scheme.Ws    => '{ Scheme.Ws }
end Origin
