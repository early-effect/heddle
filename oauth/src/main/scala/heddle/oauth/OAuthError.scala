package heddle.oauth

import heddle.client.ClientError
import heddle.crypto.RsaError
import heddle.error.HeddleError
import heddle.http.{Response, Status}
import heddle.oauth.jose.JoseError

/** Which of an authorization server's endpoints a client was talking to. */
enum ProviderEndpoint(val label: String):
  case Discovery           extends ProviderEndpoint("discovery")
  case Jwks                extends ProviderEndpoint("jwks_uri")
  case Token               extends ProviderEndpoint("token_endpoint")
  case UserInfo            extends ProviderEndpoint("userinfo_endpoint")
  case DeviceAuthorization extends ProviderEndpoint("device_authorization_endpoint")

enum OAuthError(val message: String) extends HeddleError:
  case InvalidToken(reason: JoseError) extends OAuthError(reason.message)

  /** A remote verifier has not loaded its key set yet, or the refresh failed to produce one. */
  case NoKeys extends OAuthError("the verifier has no key set yet")

  /** The platform's RSA failed: the server's fault, not the caller's. */
  case Crypto(reason: RsaError) extends OAuthError(reason.message)

  case NotConfigured(endpoint: ProviderEndpoint) extends OAuthError(s"the client has no ${endpoint.label} configured")

  /** The endpoint answered with an error status; `body` is its RFC 6749 §5.2 error document, if it sent one. */
  case Refused(endpoint: ProviderEndpoint, status: Status, body: String)
      extends OAuthError(s"${endpoint.label} answered ${status.code}: $body")

  /** The endpoint answered 2xx with a document that is not the one it should send. */
  case BadResponse(endpoint: ProviderEndpoint, detail: String)
      extends OAuthError(s"${endpoint.label} sent an unreadable response: $detail")

  /** The authorization server could not be reached, or its response body could not be read. */
  case Transport(cause: ClientError | Throwable)
      extends OAuthError(cause match
        case e: ClientError => e.message
        case t: Throwable   => t.toString)

  def toResponse: Response =
    this match
      case InvalidToken(_) | NoKeys => Response.unauthorized(message)
      case Crypto(_)                => Response.internalServerError(message)
      case _                        => Response.text(message, Status.BadGateway)
end OAuthError
