package heddle.oauth.rs

import java.time.Instant

final case class JwtClaim(
    subject: String,
    issuer: String,
    audience: List[String],
    scopes: Set[String],
    expiresAt: Instant,
    issuedAt: Instant,
    jwtId: Option[String],
)
