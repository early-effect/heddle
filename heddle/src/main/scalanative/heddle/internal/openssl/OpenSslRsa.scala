package heddle.internal.openssl

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** RS256 through OpenSSL's EVP API. `Ptr` does not leave this package; every allocation is freed on every path. */
private[heddle] object OpenSslRsa:
  import Ssl.crypto

  private val EvpPkeyRsa = 6

  /** `(n, e, d)` big-endian. */
  def generate(bits: Int): Either[SslError, (Array[Byte], Array[Byte], Array[Byte])] =
    Ssl.init()
    val ctx = crypto.EVP_PKEY_CTX_new_id(EvpPkeyRsa, null)
    if ctx == null then Left(Ssl.failed("EVP_PKEY_CTX_new_id"))
    else
      val made = Zone {
        val pp = alloc[Ptr[Byte]]()
        !pp = null
        if crypto.EVP_PKEY_keygen_init(ctx) != 1 then Left(Ssl.failed("EVP_PKEY_keygen_init"))
        else if crypto.EVP_PKEY_CTX_set_rsa_keygen_bits(ctx, bits) != 1 then
          Left(Ssl.failed("EVP_PKEY_CTX_set_rsa_keygen_bits"))
        else if crypto.EVP_PKEY_keygen(ctx, pp) != 1 || !pp == null then Left(Ssl.failed("EVP_PKEY_keygen"))
        else
          val pkey  = !pp
          val parts =
            for
              n <- bnParam(pkey, c"n")
              e <- bnParam(pkey, c"e")
              d <- bnParam(pkey, c"d")
            yield (n, e, d)
          crypto.EVP_PKEY_free(pkey)
          parts
        end if
      }
      crypto.EVP_PKEY_CTX_free(ctx)
      made
    end if
  end generate

  def sign(n: Array[Byte], e: Array[Byte], d: Array[Byte], payload: Array[Byte]): Either[SslError, Array[Byte]] =
    Ssl.init()
    withKey(n, e, Some(d))(digestSign(_, payload))

  /** `Right(false)` for a signature that does not match; `Left` when the key itself is unusable. */
  def verify(n: Array[Byte], e: Array[Byte], payload: Array[Byte], sig: Array[Byte]): Either[SslError, Boolean] =
    Ssl.init()
    withKey(n, e, None)(digestVerify(_, payload, sig))

  private def withKey[A](n: Array[Byte], e: Array[Byte], d: Option[Array[Byte]])(
      use: Ptr[Byte] => Either[SslError, A]
  ): Either[SslError, A] =
    fromRsa(n, e, d).flatMap { pkey =>
      val out = use(pkey)
      crypto.EVP_PKEY_free(pkey)
      out
    }

  /** An `EVP_PKEY` that owns its `RSA` and big numbers; a public key has no `d`. */
  private def fromRsa(n: Array[Byte], e: Array[Byte], d: Option[Array[Byte]]): Either[SslError, Ptr[Byte]] =
    val bnN             = bin2bn(n)
    val bnE             = bin2bn(e)
    val bnD             = d.fold[Ptr[Byte]](null)(bin2bn)
    val rsa             = crypto.RSA_new()
    def freeAll(): Unit =
      crypto.BN_free(bnN)
      crypto.BN_free(bnE)
      crypto.BN_free(bnD)
      if rsa != null then crypto.RSA_free(rsa)
    if bnN == null || bnE == null || (d.isDefined && bnD == null) || rsa == null then
      val err = Ssl.failed("RSA key material")
      freeAll()
      Left(err)
    else if crypto.RSA_set0_key(rsa, bnN, bnE, bnD) != 1 then
      val err = Ssl.failed("RSA_set0_key")
      freeAll()
      Left(err)
    else
      val pkey = crypto.EVP_PKEY_new()
      if pkey == null then
        val err = Ssl.failed("EVP_PKEY_new")
        crypto.RSA_free(rsa)
        Left(err)
      else if crypto.EVP_PKEY_assign(pkey, EvpPkeyRsa, rsa) != 1 then
        val err = Ssl.failed("EVP_PKEY_assign")
        crypto.EVP_PKEY_free(pkey)
        crypto.RSA_free(rsa)
        Left(err)
      else Right(pkey)
      end if
    end if
  end fromRsa

  private def digestSign(pkey: Ptr[Byte], payload: Array[Byte]): Either[SslError, Array[Byte]] =
    val ctx = crypto.EVP_MD_CTX_new()
    if ctx == null then Left(Ssl.failed("EVP_MD_CTX_new"))
    else
      val signed = Zone {
        val siglen = alloc[CSize]()
        !siglen = 0.toUSize
        val data = if payload.isEmpty then null else payload.at(0)
        if crypto.EVP_DigestSignInit(ctx, null, crypto.EVP_sha256(), null, pkey) != 1 then
          Left(Ssl.failed("EVP_DigestSignInit"))
        else if crypto.EVP_DigestSign(ctx, null, siglen, data, payload.length.toUSize) != 1 then
          Left(Ssl.failed("EVP_DigestSign"))
        else
          val sig = new Array[Byte]((!siglen).toInt.max(1))
          if crypto.EVP_DigestSign(ctx, sig.at(0), siglen, data, payload.length.toUSize) != 1 then
            Left(Ssl.failed("EVP_DigestSign"))
          else Right(sig.take((!siglen).toInt))
        end if
      }
      crypto.EVP_MD_CTX_free(ctx)
      signed
    end if
  end digestSign

  private def digestVerify(pkey: Ptr[Byte], payload: Array[Byte], sig: Array[Byte]): Either[SslError, Boolean] =
    val ctx = crypto.EVP_MD_CTX_new()
    if ctx == null then Left(Ssl.failed("EVP_MD_CTX_new"))
    else
      val verified =
        if crypto.EVP_DigestVerifyInit(ctx, null, crypto.EVP_sha256(), null, pkey) != 1 then
          Left(Ssl.failed("EVP_DigestVerifyInit"))
        else if sig.isEmpty then Right(false)
        else
          val data = if payload.isEmpty then null else payload.at(0)
          // 1 matches, 0 does not, and a negative result is a signature OpenSSL cannot parse: that does not match either.
          val rc = crypto.EVP_DigestVerify(ctx, sig.at(0), sig.length.toUSize, data, payload.length.toUSize)
          crypto.ERR_clear_error()
          Right(rc == 1)
      crypto.EVP_MD_CTX_free(ctx)
      verified
    end if
  end digestVerify

  private def bnParam(pkey: Ptr[Byte], name: CString): Either[SslError, Array[Byte]] =
    Zone {
      val bn = alloc[Ptr[Byte]]()
      !bn = null
      if crypto.EVP_PKEY_get_bn_param(pkey, name, bn) != 1 || !bn == null then Left(Ssl.failed("EVP_PKEY_get_bn_param"))
      else
        val out = bn2bin(!bn)
        crypto.BN_free(!bn)
        Right(out)
    }

  /** `null` for empty input, as OpenSSL's own allocation failure is. */
  private def bin2bn(bytes: Array[Byte]): Ptr[Byte] =
    if bytes.isEmpty then null
    else crypto.BN_bin2bn(bytes.at(0).asInstanceOf[Ptr[CUnsignedChar]], bytes.length, null)

  private def bn2bin(bn: Ptr[Byte]): Array[Byte] =
    val n = (crypto.BN_num_bits(bn) + 7) / 8
    val a = new Array[Byte](n.max(1))
    val _ = crypto.BN_bn2bin(bn, a.at(0).asInstanceOf[Ptr[CUnsignedChar]])
    a
end OpenSslRsa
