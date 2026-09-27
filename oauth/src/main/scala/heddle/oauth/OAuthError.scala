package heddle.oauth

import heddle.client.ClientError
import heddle.http.Response

enum OAuthError:
  case InvalidToken(detail: String)
  case Discovery(detail: String)
  case Protocol(detail: String)

  /** The authorization server could not be reached, or its response could not be read. */
  case Transport(cause: ClientError | Throwable)

  def message: String =
    this match
      case InvalidToken(m) => m
      case Discovery(m)    => m
      case Protocol(m)     => m
      case Transport(c)    =>
        c match
          case e: ClientError => e.message
          case t: Throwable   => Option(t.getMessage).getOrElse(t.toString)

  def toResponse: Response =
    this match
      case InvalidToken(_) => Response.unauthorized(message)
      case _               => Response.badRequest(message)
end OAuthError
