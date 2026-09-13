package heddle.http

import heddle.internal.Ascii
import zio.Chunk

final case class Url(
    path: Path,
    query: QueryParams = QueryParams.empty,
    scheme: Option[String] = None,
    host: Option[String] = None,
    port: Option[Int] = None,
):
  def /(segment: String): Url = copy(path = path / segment)

  def absolute: Boolean = host.isDefined

  def render: String =
    val pq =
      if query.isEmpty then path.render
      else s"${path.render}?${query.render}"
    host match
      case None    => pq
      case Some(h) =>
        val sch = scheme.getOrElse("http")
        val p   = port.filterNot(n => (sch == "http" && n == 80) || (sch == "https" && n == 443))
        val ps  = p.map(n => s":$n").getOrElse("")
        s"$sch://$h$ps$pq"
  end render
end Url

object Url:
  val root: Url = Url(Path.root)

  def parse(raw: String): Url =
    if raw.startsWith("http://") || raw.startsWith("https://") then
      val u   = java.net.URI(raw)
      val pth = Path.decode(Option(u.getRawPath).filter(_.nonEmpty).getOrElse("/"))
      val q   = Option(u.getRawQuery).fold(QueryParams.empty)(QueryParams.decode)
      val prt = if u.getPort > 0 then Some(u.getPort) else None
      Url(pth, q, Option(u.getScheme), Option(u.getHost), prt)
    else
      val bytes = Chunk.fromArray(raw.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      parse(bytes, 0, bytes.length)

  def parse(raw: Chunk[Byte], from: Int, until: Int): Url =
    var q = from
    while q < until && raw(q) != '?' do q += 1
    val path = Path.decode(raw, from, q)
    if q < until then Url(path, QueryParams.decode(Ascii.string(raw, q + 1, until)))
    else Url(path, QueryParams.empty)
end Url
