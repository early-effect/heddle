package heddle.oauth.jose

import heddle.crypto.{Base64Url, RsaKey, RsaPlatform, RsaPublic}
import heddle.oauth.rs.JwtClaim
import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

final case class Jwk(kid: String, n: Chunk[Byte], e: Chunk[Byte]):
  def public: RsaPublic = RsaPublic(n, e)

final case class Jwks(keys: List[Jwk]):
  def byKid(kid: Option[String]): Option[Jwk] =
    kid.flatMap(id => keys.find(_.kid == id)).orElse(keys.headOption)

final case class SigningKey(kid: String, key: RsaKey):
  def publicJwk: Jwk = Jwk(kid, key.public.n, key.public.e)

  def jwks: Jwks = Jwks(List(publicJwk))

  def publicJwksJson: String =
    val n = Base64Url.encode(key.public.n)
    val e = Base64Url.encode(key.public.e)
    Json
      .Obj(
        "keys" -> Json.Arr(
          Json.Obj(
            "kty" -> Json.Str("RSA"),
            "kid" -> Json.Str(kid),
            "use" -> Json.Str("sig"),
            "alg" -> Json.Str("RS256"),
            "n"   -> Json.Str(n),
            "e"   -> Json.Str(e),
          )
        )
      )
      .toJson
  end publicJwksJson
end SigningKey

object SigningKey:
  def generateRsa: SigningKey = generateRsa(heddle.internal.Ids.uuid().toString)

  def generateRsa(kid: String): SigningKey =
    SigningKey(kid, RsaPlatform.generateSync(2048))

object Jose:
  def sign(
      key: SigningKey,
      subject: String,
      issuer: String,
      audience: String,
      scopes: Set[String],
      ttl: zio.Duration,
  ): String =
    sign(key, subject, issuer, audience, scopes, ttl, Map.empty)

  def sign(
      key: SigningKey,
      subject: String,
      issuer: String,
      audience: String,
      scopes: Set[String],
      ttl: zio.Duration,
      extra: Map[String, String],
  ): String =
    val now    = Instant.now()
    val header = Json.Obj(
      "alg" -> Json.Str("RS256"),
      "typ" -> Json.Str("JWT"),
      "kid" -> Json.Str(key.kid),
    )
    val claims =
      Chunk(
        "sub"   -> Json.Str(subject),
        "iss"   -> Json.Str(issuer),
        "aud"   -> Json.Str(audience),
        "iat"   -> Json.Num(now.getEpochSecond),
        "exp"   -> Json.Num(now.getEpochSecond + ttl.toSeconds),
        "jti"   -> Json.Str(heddle.internal.Ids.uuid().toString),
        "scope" -> Json.Str(scopes.mkString(" ")),
      ) ++ Chunk.fromIterable(extra.toList.map((k, v) => k -> Json.Str(v)))
    val payload = Json.Obj(claims)
    val hB64    = Base64Url.encode(utf8(header.toJson))
    val pB64    = Base64Url.encode(utf8(payload.toJson))
    val input   = s"$hB64.$pB64"
    val sig     = RsaPlatform.signSync(key.key, utf8(input))
    s"$input.${Base64Url.encode(sig)}"
  end sign

  def verify(token: String, jwks: Jwks, issuer: String, audience: String): Either[JoseError, JwtClaim] =
    for
      parts <- splitCompact(token)
      (hB64, pB64, sB64) = parts
      header <- decodeJson("header", hB64)
      alg    <- str(header, "alg").toRight(JoseError.MissingAlg)
      _      <- Either.cond(alg == "RS256", (), JoseError.UnsupportedAlg(alg))
      jwk    <- jwks.byKid(str(header, "kid")).toRight(JoseError.NoMatchingKey)
      sig    <- Base64Url.decode(sB64).left.map(JoseError.BadBase64("signature", _))
      input = utf8(s"$hB64.$pB64")
      _       <- Either.cond(RsaPlatform.verifySync(jwk.public, input, sig), (), JoseError.BadSignature)
      payload <- decodeJson("payload", pB64)
      claim   <- claimsOf(payload, issuer, audience)
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

  private def claimsOf(payload: Json.Obj, issuer: String, audience: String): Either[JoseError, JwtClaim] =
    val now = Instant.now()
    val exp = num(payload, "exp").map(Instant.ofEpochSecond)
    val nbf = num(payload, "nbf").map(Instant.ofEpochSecond)
    val iss = str(payload, "iss").getOrElse("")
    val aud = audienceOf(payload)
    if exp.exists(now.isAfter) then Left(JoseError.Expired)
    else if nbf.exists(now.isBefore) then Left(JoseError.NotYetValid)
    else if issuer.nonEmpty && iss != issuer then Left(JoseError.IssuerMismatch(issuer, iss))
    else if audience.nonEmpty && !aud.contains(audience) then Left(JoseError.AudienceMismatch(audience, aud))
    else
      val scope = str(payload, "scope").getOrElse("")
      Right(
        JwtClaim(
          subject = str(payload, "sub").getOrElse(""),
          issuer = iss,
          audience = aud,
          scopes = scope.split(" ").filter(_.nonEmpty).toSet,
          expiresAt = exp.getOrElse(now),
          issuedAt = num(payload, "iat").map(Instant.ofEpochSecond).getOrElse(now),
          jwtId = str(payload, "jti"),
        )
      )
    end if
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
