package heddle.oauth.jose

import heddle.crypto.{Base64Error, RsaError}
import heddle.error.HeddleError

/** Why a compact JWT or a JWKS was refused. */
enum JoseError(val message: String) extends HeddleError:
  case Malformed                                    extends JoseError("not a compact JWT: header.payload.signature")
  case BadBase64(part: String, reason: Base64Error) extends JoseError(s"the $part is not base64url: ${reason.message}")
  case NotJsonObject(part: String)                  extends JoseError(s"the $part is not a JSON object")
  case MissingAlg                                   extends JoseError("the header names no alg")
  case UnsupportedAlg(alg: String)                  extends JoseError(s"alg $alg is not RS256")
  case NoMatchingKey                                extends JoseError("no key in the JWKS has the token's kid")
  case KeyUnnamed                 extends JoseError("the token names no kid, and the JWKS holds more than one key")
  case MissingClaim(name: String) extends JoseError(s"the token has no $name claim")
  case BadSignature               extends JoseError("the signature does not verify")
  case Expired                    extends JoseError("expired")
  case NotYetValid                extends JoseError("not yet valid")
  case IssuerMismatch(expected: String, actual: String) extends JoseError(s"issued by '$actual', expected '$expected'")
  case AudienceMismatch(expected: String, actual: List[String])
      extends JoseError(s"for ${actual.mkString(", ")}, expected $expected")
  case JwksNotJson(detail: String) extends JoseError(s"the JWKS is not JSON: $detail")
  case JwkNotRsa(kty: String)      extends JoseError(s"a JWK has kty $kty, not RSA")
  case JwkMissing(field: String)   extends JoseError(s"a JWK has no $field")
  case NoKeys                      extends JoseError("the JWKS has no keys")

  /** The platform's RSA failed while checking the signature. */
  case Crypto(reason: RsaError) extends JoseError(reason.message)
end JoseError
