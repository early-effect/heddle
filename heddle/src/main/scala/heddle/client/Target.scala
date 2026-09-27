package heddle.client

import heddle.http.{Request, Scheme, Url}
import heddle.http.header.HeaderName

/** Where one HTTP request goes, resolved from its `Url` (absolute-form) or its `Host` header (origin-form). */
private[heddle] final case class Target(scheme: Scheme, authority: Authority, requestTarget: String):
  def tls: Boolean = scheme == Scheme.Https

  def hostHeader: String =
    val host = if authority.host.contains(':') then s"[${authority.host}]" else authority.host
    if authority.port == scheme.defaultPort then host else s"$host:${authority.port}"

  def url: String = s"${scheme.render}://$hostHeader$requestTarget"

private[heddle] object Target:
  def of(req: Request): Either[ClientError, Target] =
    if req.url.absolute then of(req.url)
    else
      req.header(HeaderName.Host) match
        case Some(host) =>
          val scheme = if req.secure then Scheme.Https else Scheme.Http
          of(Url.parse(s"${scheme.render}://$host${req.url.render}"))
        case None =>
          Left(ClientError.InvalidTarget(req.url.render, TargetError.NoHost))

  def of(url: Url): Either[ClientError, Target] =
    val scheme = url.scheme.getOrElse(Scheme.Http)
    url.host.filter(_.nonEmpty) match
      case None => Left(ClientError.InvalidTarget(url.render, TargetError.NoHost))
      case Some(_) if scheme != Scheme.Http && scheme != Scheme.Https =>
        Left(ClientError.InvalidTarget(url.render, TargetError.NotHttp(scheme)))
      case Some(host) =>
        val bare = host.stripPrefix("[").stripSuffix("]")
        val rt   = if url.query.isEmpty then url.path.render else s"${url.path.render}?${url.query.render}"
        Right(Target(scheme, Authority(bare, url.port.getOrElse(scheme.defaultPort)), rt))
  end of
end Target
