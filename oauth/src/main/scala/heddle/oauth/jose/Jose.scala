package heddle.oauth.jose

import heddle.crypto.{Base64Url, Rsa, RsaError, RsaKey, RsaPublic}
import heddle.internal.Ids
import heddle.oauth.rs.JwtClaim
import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.{Chunk, Clock, Duration, ZIO}
import zio.json.*
import zio.json.ast.Json

final case class Jwk(kid: String, n: Chunk[Byte], e: Chunk[Byte]):
  def public: RsaPublic = RsaPublic(n, e)

final case class Jwks(keys: List[Jwk]):
  /** The key a token's header names. With no `kid`, only a set of exactly one key answers. */
  def byKid(kid: Option[String]): Either[JoseError, Jwk] =
    kid match
      case Some(id) => keys.find(_.kid == id).toRight(JoseError.NoMatchingKey)
      case None     =>
        keys match
          case only :: Nil => Right(only)
          case _           => Left(JoseError.KeyUnnamed)

final case class SigningKey(kid: String, key: RsaKey):
  def publicJwk: Jwk = Jwk(kid, key.public.n, key.public.e)

  def jwks: Jwks = Jwks(List(publicJwk))

  def publicJwksJson: String =
    Json
      .Obj(
        "keys" -> Json.Arr(
          Json.Obj(
            "kty" -> Json.Str("RSA"),
            "kid" -> Json.Str(kid),
            "use" -> Json.Str("sig"),
            "alg" -> Json.Str("RS256"),
            "n"   -> Json.Str(Base64Url.encode(key.public.n)),
            "e"   -> Json.Str(Base64Url.encode(key.public.e)),
          )
        )
      )
      .toJson
end SigningKey

object SigningKey:
  /** A 2048-bit key under a random `kid`. */
  val generateRsa: ZIO[Rsa, RsaError, SigningKey] =
    Ids.uuid.flatMap(id => generateRsa(id.toString))

  def generateRsa(kid: String): ZIO[Rsa, RsaError, SigningKey] =
    Rsa.generate(2048).map(SigningKey(kid, _))

/** What a signed token says. `iat`, `exp`, and `jti` come from the clock and the CSPRNG when it is signed. */
final case class TokenClaims(
    subject: String,
    issuer: String,
    audience: String,
    scopes: Set[String],
    ttl: Duration,
    extra: Map[String, String] = Map.empty,
)

/** Where a verified token must come from and who it must be for. */
final case class Expected(issuer: String, audience: Option[String])

object Jose:
  def sign(key: SigningKey, claims: TokenClaims): ZIO[Rsa, RsaError, String] =
    for
      now <- Clock.instant
      jti <- Ids.uuid
      input = signingInput(key, claims, now, jti.toString)
      sig <- Rsa.signSha256(key.key, utf8(input))
    yield s"$input.${Base64Url.encode(sig)}"

  def verify(token: String, jwks: Jwks, expected: Expected): ZIO[Rsa, JoseError, JwtClaim] =
    for
      parts <- ZIO.fromEither(signed(token, jwks))
      (input, sig, jwk, pB64) = parts
      ok      <- Rsa.verifySha256(jwk.public, input, sig).mapError(JoseError.Crypto(_))
      _       <- ZIO.fail(JoseError.BadSignature).unless(ok)
      payload <- ZIO.fromEither(decodeJson("payload", pB64))
      now     <- Clock.instant
      claim   <- ZIO.fromEither(claimsOf(payload, expected, now))
    yield claim

  def parseJwks(json: String): Either[JoseError, Jwks] =
    json.fromJson[JwksWire].left.map(JoseError.JwksNotJson(_)).flatMap { wire =>
      val parsed = wire.keys.map { k =>
        k.kty.filter(_ != "RSA") match
          case Some(kty) => Left(JoseError.JwkNotRsa(kty))
          case None      =>
            for
              n <- k.n.toRight(JoseError.JwkMissing("n")).flatMap(key("n"))
              e <- k.e.toRight(JoseError.JwkMissing("e")).flatMap(key("e"))
            yield Jwk(k.kid.getOrElse(""), n, e)
      }
      parsed.collectFirst { case Left(err) => err } match
        case Some(err) => Left(err)
        case None      =>
          val keys = parsed.collect { case Right(j) => j }
          if keys.isEmpty then Left(JoseError.NoKeys) else Right(Jwks(keys))
    }

  private def signingInput(key: SigningKey, claims: TokenClaims, now: Instant, jti: String): String =
    val header = Json.Obj("alg" -> Json.Str("RS256"), "typ" -> Json.Str("JWT"), "kid" -> Json.Str(key.kid))
    val body   =
      Chunk(
        "sub"   -> Json.Str(claims.subject),
        "iss"   -> Json.Str(claims.issuer),
        "aud"   -> Json.Str(claims.audience),
        "iat"   -> Json.Num(now.getEpochSecond),
        "exp"   -> Json.Num(now.getEpochSecond + claims.ttl.toSeconds),
        "jti"   -> Json.Str(jti),
        "scope" -> Json.Str(claims.scopes.mkString(" ")),
      ) ++ Chunk.fromIterable(claims.extra.toList.map((k, v) => k -> Json.Str(v)))
    s"${Base64Url.encode(utf8(header.toJson))}.${Base64Url.encode(utf8(Json.Obj(body).toJson))}"
  end signingInput

  /** The signed bytes, the signature, the key the header names, and the still-encoded payload. */
  private def signed(token: String, jwks: Jwks): Either[JoseError, (Chunk[Byte], Chunk[Byte], Jwk, String)] =
    for
      parts <- splitCompact(token)
      (hB64, pB64, sB64) = parts
      header <- decodeJson("header", hB64)
      alg    <- str(header, "alg").toRight(JoseError.MissingAlg)
      _      <- Either.cond(alg == "RS256", (), JoseError.UnsupportedAlg(alg))
      jwk    <- jwks.byKid(str(header, "kid"))
      sig    <- Base64Url.decode(sB64).left.map(JoseError.BadBase64("signature", _))
    yield (utf8(s"$hB64.$pB64"), sig, jwk, pB64)

  private def key(field: String)(b64: String): Either[JoseError, Chunk[Byte]] =
    Base64Url.decode(b64).left.map(JoseError.BadBase64(s"JWK $field", _))

  private def splitCompact(token: String): Either[JoseError, (String, String, String)] =
    token.split("\\.", -1) match
      case Array(h, p, s) if h.nonEmpty && p.nonEmpty && s.nonEmpty => Right((h, p, s))
      case _                                                        => Left(JoseError.Malformed)

  private def decodeJson(part: String, b64: String): Either[JoseError, Json.Obj] =
    Base64Url.decode(b64).left.map(JoseError.BadBase64(part, _)).flatMap { bytes =>
      String(bytes.toArray, StandardCharsets.UTF_8).fromJson[Json] match
        case Right(o: Json.Obj) => Right(o)
        case _                  => Left(JoseError.NotJsonObject(part))
    }

  /** RFC 9068 §4: `sub` and `exp` are required, `iss` must match, and `aud` must name the expected audience. */
  private def claimsOf(payload: Json.Obj, expected: Expected, now: Instant): Either[JoseError, JwtClaim] =
    val aud = audienceOf(payload)
    val iss = str(payload, "iss").getOrElse("")
    for
      subject <- str(payload, "sub").toRight(JoseError.MissingClaim("sub"))
      exp     <- num(payload, "exp").map(Instant.ofEpochSecond).toRight(JoseError.MissingClaim("exp"))
      _       <- Either.cond(!now.isAfter(exp), (), JoseError.Expired)
      _ <- Either.cond(!num(payload, "nbf").map(Instant.ofEpochSecond).exists(now.isBefore), (), JoseError.NotYetValid)
      _ <- Either.cond(iss == expected.issuer, (), JoseError.IssuerMismatch(expected.issuer, iss))
      _ <- expected.audience.filterNot(aud.contains).map(a => JoseError.AudienceMismatch(a, aud)).toLeft(())
    yield JwtClaim(
      subject = subject,
      issuer = iss,
      audience = aud,
      scopes = str(payload, "scope").getOrElse("").split(" ").filter(_.nonEmpty).toSet,
      expiresAt = exp,
      issuedAt = num(payload, "iat").map(Instant.ofEpochSecond),
      jwtId = str(payload, "jti"),
    )
    end for
  end claimsOf

  private def audienceOf(payload: Json.Obj): List[String] =
    payload.get("aud") match
      case Some(Json.Str(s))  => List(s)
      case Some(Json.Arr(xs)) => xs.collect { case Json.Str(s) => s }.toList
      case _                  => Nil

  private def str(obj: Json.Obj, name: String): Option[String] =
    obj.get(name).collect { case Json.Str(s) => s }

  private def num(obj: Json.Obj, name: String): Option[Long] =
    obj.get(name).collect { case Json.Num(n) => n.longValue }

  private def utf8(s: String): Chunk[Byte] =
    Chunk.fromArray(s.getBytes(StandardCharsets.UTF_8))

  private final case class JwkWire(kty: Option[String], kid: Option[String], n: Option[String], e: Option[String])
      derives JsonDecoder

  private final case class JwksWire(keys: List[JwkWire]) derives JsonDecoder
end Jose
