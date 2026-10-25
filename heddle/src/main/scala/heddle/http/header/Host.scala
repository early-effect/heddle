package heddle.http.header

import heddle.error.ParamError

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

  given TypedHeader[Host] =
    TypedHeader.single(HeaderName.Host)(
      raw => parse(raw).toRight(ParamError.Malformed(raw, "host[:port]")),
      host =>
        host.port.fold(host.host)(p => if host.host.contains(':') then s"[${host.host}]:$p" else s"${host.host}:$p"),
    )
end Host
