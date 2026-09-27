package heddle.oauth

import heddle.client.ClientError
import heddle.error.HeddleError
import heddle.http.Response
import heddle.oauth.jose.JoseError

enum OAuthError extends HeddleError:
  case InvalidToken(reason: JoseError)

  /** A remote verifier has not loaded its key set yet, or the refresh failed to produce one. */
  case NoKeys
  case Discovery(detail: String)
  case Protocol(detail: String)

  /** The authorization server could not be reached, or its response could not be read. */
  case Transport(cause: ClientError | Throwable)

  def message: String =
    this match
      case InvalidToken(e) => e.message
      case NoKeys          => "the verifier has no key set yet"
      case Discovery(m)    => m
      case Protocol(m)     => m
      case Transport(c)    =>
        c match
          case e: ClientError => e.message
          case t: Throwable   => Option(t.getMessage).getOrElse(t.toString)

  def toResponse: Response =
    this match
      case InvalidToken(_) | NoKeys => Response.unauthorized(message)
      case _                        => Response.badRequest(message)
end OAuthError
