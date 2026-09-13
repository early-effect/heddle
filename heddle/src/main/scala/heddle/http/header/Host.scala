package heddle.http.header

final case class Host(host: String, port: Option[Int] = None)

object Host:
  def parse(raw: String): Option[Host] =
    val s = raw.trim
    if s.isEmpty then None
    else if s.startsWith("[") then
      val close = s.indexOf(']')
      if close < 0 then None
      else
        val h    = s.substring(1, close)
        val rest = s.substring(close + 1)
        if rest.isEmpty then Some(Host(h, None))
        else if rest.startsWith(":") then rest.substring(1).toIntOption.map(p => Host(h, Some(p)))
        else None
    else
      val colon = s.lastIndexOf(':')
      if colon < 0 then Some(Host(s, None))
      else
        val h = s.substring(0, colon)
        val p = s.substring(colon + 1)
        if h.isEmpty then None
        else p.toIntOption.map(n => Host(h, Some(n))).orElse(Some(Host(s, None)))
    end if
  end parse

  given TypedHeader[Host] with
    def name: HeaderName                          = HeaderName.Host
    def decode(raw: String): Either[String, Host] =
      parse(raw).toRight("invalid Host")
    def encode(a: Host): String =
      a.port.fold(a.host)(p => if a.host.contains(':') then s"[${a.host}]:$p" else s"${a.host}:$p")
end Host
