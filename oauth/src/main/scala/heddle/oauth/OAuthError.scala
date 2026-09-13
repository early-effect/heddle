package heddle.oauth

import heddle.http.Response

enum OAuthError:
  case InvalidToken(detail: String)
  case Discovery(detail: String)
  case Protocol(detail: String)
  case Transport(cause: Throwable)

  def message: String =
    this match
      case InvalidToken(m) => m
      case Discovery(m)    => m
      case Protocol(m)     => m
      case Transport(c)    => Option(c.getMessage).getOrElse(c.toString)

  def toResponse: Response =
    this match
      case InvalidToken(_) => Response.unauthorized(message)
      case _               => Response.badRequest(message)
end OAuthError
