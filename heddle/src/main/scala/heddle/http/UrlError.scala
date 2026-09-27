package heddle.http

import heddle.error.HeddleError

/** Why text is not a URL `Url.decode` accepts. */
enum UrlError(val message: String) extends HeddleError:
  case Empty                             extends UrlError("empty URL")
  case BadCharacter(char: Char, at: Int) extends UrlError(s"'$char' at $at cannot appear in a URL")
  case UnknownScheme(scheme: String)     extends UrlError(s"$scheme is not a scheme heddle knows")
  case BadAuthority(authority: String)   extends UrlError(s"'$authority' is not host[:port]")
