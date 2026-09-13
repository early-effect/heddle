package heddle.oauth.jose

import com.nimbusds.jose.jwk.gen.RSAKeyGenerator
import com.nimbusds.jose.jwk.{JWKSet, RSAKey}
import com.nimbusds.jose.crypto.{RSASSASigner, RSASSAVerifier}
import com.nimbusds.jose.{JWSAlgorithm, JWSHeader}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import heddle.oauth.rs.JwtClaim
import java.time.Instant
import java.util.Date
import scala.jdk.CollectionConverters.*

final case class SigningKey(kid: String, privateKey: RSAKey):
  def publicJwksJson: String = new JWKSet(privateKey.toPublicJWK).toString
  def jwkSet: JWKSet         = new JWKSet(privateKey.toPublicJWK)

object SigningKey:
  def generateRsa(kid: String = java.util.UUID.randomUUID().toString): SigningKey =
    val key = RSAKeyGenerator(2048).keyID(kid).generate()
    SigningKey(kid, key)

object Jose:
  def sign(
      key: SigningKey,
      subject: String,
      issuer: String,
      audience: String,
      scopes: Set[String],
      ttl: zio.Duration,
      extra: Map[String, String] = Map.empty,
  ): String =
    val now    = Instant.now()
    val claims = JWTClaimsSet
      .Builder()
      .subject(subject)
      .issuer(issuer)
      .audience(audience)
      .issueTime(Date.from(now))
      .expirationTime(Date.from(now.plusMillis(ttl.toMillis)))
      .jwtID(java.util.UUID.randomUUID().toString)
      .claim("scope", scopes.mkString(" "))
    extra.foreach((k, v) => claims.claim(k, v))
    val jwt = SignedJWT(JWSHeader.Builder(JWSAlgorithm.RS256).keyID(key.kid).build(), claims.build())
    jwt.sign(RSASSASigner(key.privateKey))
    jwt.serialize()
  end sign

  def verify(token: String, jwks: JWKSet, issuer: String, audience: String): Either[String, JwtClaim] =
    try
      val jwt = SignedJWT.parse(token)
      val kid = Option(jwt.getHeader.getKeyID)
      val jwk = kid.flatMap(id => Option(jwks.getKeyByKeyId(id))).orElse(jwks.getKeys.asScala.headOption)
      jwk match
        case None    => Left("no matching JWK")
        case Some(k) =>
          val rsa = k.asInstanceOf[RSAKey]
          if !jwt.verify(RSASSAVerifier(rsa)) then Left("bad signature")
          else
            val c   = jwt.getJWTClaimsSet
            val exp = Option(c.getExpirationTime).map(_.toInstant)
            val nbf = Option(c.getNotBeforeTime).map(_.toInstant)
            val now = Instant.now()
            if exp.exists(now.isAfter) then Left("expired")
            else if nbf.exists(now.isBefore) then Left("not yet valid")
            else if Option(c.getIssuer).exists(_ != issuer) then Left("issuer mismatch")
            else
              val aud = Option(c.getAudience).map(_.asScala.toList).getOrElse(Nil)
              if audience.nonEmpty && !aud.contains(audience) && Option(c.getAudience).isDefined then
                Left("audience mismatch")
              else
                val scope = Option(c.getStringClaim("scope")).getOrElse("")
                Right(
                  JwtClaim(
                    subject = Option(c.getSubject).getOrElse(""),
                    issuer = Option(c.getIssuer).getOrElse(""),
                    audience = aud,
                    scopes = scope.split(" ").filter(_.nonEmpty).toSet,
                    expiresAt = exp.getOrElse(now),
                    issuedAt = Option(c.getIssueTime).map(_.toInstant).getOrElse(now),
                    jwtId = Option(c.getJWTID),
                  )
                )
              end if
            end if
          end if
      end match
    catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))

  def parseJwks(json: String): Either[String, JWKSet] =
    try Right(JWKSet.parse(json))
    catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))
end Jose
