package heddle.internal.openssl

import heddle.internal.posix.{Interest, Transfer}
import java.io.IOException
import scala.annotation.unused
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** OpenSSL externs and byte-level helpers. `Ptr` does not leave this package. */
private[heddle] object Ssl:
  def sha256(bytes: Array[Byte]): Array[Byte] =
    val out = new Array[Byte](32)
    if bytes.isEmpty then
      val _ = crypto.SHA256(null, 0.toUSize, out.at(0).asInstanceOf[Ptr[CUnsignedChar]])
    else
      val _ = crypto.SHA256(
        bytes.at(0).asInstanceOf[Ptr[CUnsignedChar]],
        bytes.length.toUSize,
        out.at(0).asInstanceOf[Ptr[CUnsignedChar]],
      )
    out
  end sha256

  def rsaGenerate(bits: Int): (Array[Byte], Array[Byte], Array[Byte]) =
    init()
    Zone {
      val ctx = crypto.EVP_PKEY_CTX_new_id(EvpPkeyRsa, null)
      if ctx == null then throw fail("EVP_PKEY_CTX_new_id")
      try
        if crypto.EVP_PKEY_keygen_init(ctx) != 1 then throw fail("EVP_PKEY_keygen_init")
        if crypto.EVP_PKEY_CTX_set_rsa_keygen_bits(ctx, bits) != 1 then throw fail("rsa_keygen_bits")
        val pp = alloc[Ptr[Byte]]()
        !pp = null
        if crypto.EVP_PKEY_keygen(ctx, pp) != 1 || !pp == null then throw fail("EVP_PKEY_keygen")
        val pkey = !pp
        try
          val n = bnParam(pkey, c"n")
          val e = bnParam(pkey, c"e")
          val d = bnParam(pkey, c"d")
          (n, e, d)
        finally crypto.EVP_PKEY_free(pkey)
      finally crypto.EVP_PKEY_CTX_free(ctx)
      end try
    }
  end rsaGenerate

  def rsaSign(n: Array[Byte], e: Array[Byte], d: Array[Byte], payload: Array[Byte]): Array[Byte] =
    init()
    val pkey = fromRsa(n, e, Some(d))
    try digestSign(pkey, payload)
    finally crypto.EVP_PKEY_free(pkey)

  def rsaVerify(n: Array[Byte], e: Array[Byte], payload: Array[Byte], sig: Array[Byte]): Boolean =
    init()
    val pkey = fromRsa(n, e, None)
    try digestVerify(pkey, payload, sig)
    finally crypto.EVP_PKEY_free(pkey)

  final class Ctx private[openssl] (
      private[openssl] val ptr: Ptr[Byte],
      @unused private val pinned: Array[Array[Byte]],
  ):
    def close(): Unit = if ptr != null then ssl.SSL_CTX_free(ptr)

  final class Session private[openssl] (
      private var ptr: Ptr[Byte],
      @unused private val ctx: Ctx,
  ):
    private val scratch = new Array[Byte](16384)

    private[openssl] def ptrOrNull: Ptr[Byte] = ptr

    def close(): Unit =
      this.synchronized {
        if ptr != null then
          ssl.SSL_free(ptr)
          ptr = null
      }

    def read(dst: Array[Byte], off: Int, len: Int): Either[SslError, Transfer] =
      if ptr == null then Right(Transfer.Eof)
      else if len <= 0 then Right(Transfer.Moved(0))
      else
        val n = ssl.SSL_read(ptr, scratch.at(0), math.min(len, scratch.length))
        if n <= 0 then outcome(n, "SSL_read")
        else
          System.arraycopy(scratch, 0, dst, off, n)
          Right(Transfer.Moved(n))

    def write(src: Array[Byte], off: Int, len: Int): Either[SslError, Transfer] =
      if ptr == null then Left(SslError.ClosedSession)
      else if len <= 0 then Right(Transfer.Moved(0))
      else
        val n = math.min(len, scratch.length)
        System.arraycopy(src, off, scratch, 0, n)
        val wrote = ssl.SSL_write(ptr, scratch.at(0), n)
        if wrote <= 0 then outcome(wrote, "SSL_write") else Right(Transfer.Moved(wrote))

    /** `None` once the handshake is done, or the readiness it waits for. */
    def handshake(accept: Boolean): Either[SslError, Option[Interest]] =
      val function = if accept then "SSL_accept" else "SSL_connect"
      if ptr == null then Left(SslError.ClosedSession)
      else
        val n = if accept then ssl.SSL_accept(ptr) else ssl.SSL_connect(ptr)
        if n == 1 then Right(None)
        else
          outcome(n, function).flatMap {
            case Transfer.Blocked(on) => Right(Some(on))
            case _                    => Left(SslError.PeerClosed(function))
          }
    end handshake

    private def outcome(n: Int, function: String): Either[SslError, Transfer] =
      ssl.SSL_get_error(ptr, n) match
        case ErrorZeroReturn => Right(Transfer.Eof)
        case ErrorWantRead   => Right(Transfer.Blocked(Interest.Read))
        case ErrorWantWrite  => Right(Transfer.Blocked(Interest.Write))
        case code            => Left(failed(s"$function (SSL_get_error $code)"))
  end Session

  def serverCtx(certPem: String, keyPem: String): Either[SslError, Ctx] =
    init()
    val ctx = ssl.SSL_CTX_new(ssl.TLS_server_method())
    if ctx == null then Left(failed("SSL_CTX_new"))
    else
      loadPem(ctx, certPem, keyPem) match
        case Left(e) =>
          ssl.SSL_CTX_free(ctx)
          Left(e)
        case Right(pinned) => Right(Ctx(ctx, pinned))
  end serverCtx

  /** Verifies the peer against `trustPem` when given, otherwise against the system's default trust store. */
  def clientCtx(trustPem: Option[String]): Either[SslError, Ctx] =
    init()
    val ctx = ssl.SSL_CTX_new(ssl.TLS_client_method())
    if ctx == null then Left(failed("SSL_CTX_new"))
    else
      ssl.SSL_CTX_set_verify(ctx, VerifyPeer, null)
      val trusted = trustPem match
        case None =>
          if ssl.SSL_CTX_set_default_verify_paths(ctx) == 1 then Right(())
          else Left(failed("SSL_CTX_set_default_verify_paths"))
        case Some(pem) => trust(ctx, pem)
      trusted match
        case Left(e) =>
          ssl.SSL_CTX_free(ctx)
          Left(e)
        case Right(_) => Right(Ctx(ctx, Array.empty))
    end if
  end clientCtx

  def accept(ctx: Ctx, fd: Int): Either[SslError, Session] = attach(ctx, fd, host = None)

  /** A client session that checks the peer certificate names `host` (a DNS name or an IP literal). */
  def connect(ctx: Ctx, fd: Int, host: String): Either[SslError, Session] =
    attach(ctx, fd, Some(host)).flatMap { session =>
      val ok = Zone {
        if isIpLiteral(host) then
          crypto.X509_VERIFY_PARAM_set1_ip_asc(ssl.SSL_get0_param(session.ptrOrNull), toCString(host))
        else ssl.SSL_set1_host(session.ptrOrNull, toCString(host))
      }
      if ok == 1 then Right(session)
      else
        val e = failed("set verify host")
        session.close()
        Left(e)
    }

  private def isIpLiteral(host: String): Boolean =
    host.contains(':') || host.forall(c => c.isDigit || c == '.')

  private def trust(ctx: Ptr[Byte], pem: String): Either[SslError, Unit] =
    val store = ssl.SSL_CTX_get_cert_store(ctx)
    withBio(ascii(pem)) { bio =>
      var added = 0
      var cert  = crypto.PEM_read_bio_X509(bio, null, null, null)
      while cert != null do
        if crypto.X509_STORE_add_cert(store, cert) == 1 then added += 1
        crypto.X509_free(cert)
        cert = crypto.PEM_read_bio_X509(bio, null, null, null)
      crypto.ERR_clear_error()
      if added == 0 then Left(SslError.NoTrustedCertificates) else Right(())
    }
  end trust

  def handshake(session: Session, accept: Boolean): Either[SslError, Option[Interest]] = session.handshake(accept)

  private def attach(ctx: Ctx, fd: Int, host: Option[String]): Either[SslError, Session] =
    val s = ssl.SSL_new(ctx.ptr)
    if s == null then Left(failed("SSL_new"))
    else if ssl.SSL_set_fd(s, fd) != 1 then
      val e = failed("SSL_set_fd")
      ssl.SSL_free(s)
      Left(e)
    else
      host.filterNot(isIpLiteral).foreach { name =>
        Zone {
          val _ = ssl.SSL_ctrl(s, CtrlSetTlsextHostname, 0, toCString(name).asInstanceOf[Ptr[Byte]])
        }
      }
      Right(Session(s, ctx))
    end if
  end attach

  private val EvpPkeyRsa = 6

  private def init(): Unit =
    val _ = ssl.OPENSSL_init_ssl(0.toUSize.asInstanceOf[CUnsignedLong], null)

  private def ascii(s: String): Array[Byte] =
    val a = new Array[Byte](s.length)
    var i = 0
    while i < s.length do
      a(i) = s.charAt(i).toByte
      i += 1
    a

  private def loadPem(ctx: Ptr[Byte], certPem: String, keyPem: String): Either[SslError, Array[Array[Byte]]] =
    val certBytes = ascii(certPem)
    val keyBytes  = ascii(keyPem)
    withBio(certBytes) { certBio =>
      withBio(keyBytes) { keyBio =>
        val cert = crypto.PEM_read_bio_X509(certBio, null, null, null)
        val key  = crypto.PEM_read_bio_PrivateKey(keyBio, null, null, null)
        val used =
          if cert == null then Left(failed("PEM_read_bio_X509"))
          else if key == null then Left(failed("PEM_read_bio_PrivateKey"))
          else if ssl.SSL_CTX_use_certificate(ctx, cert) != 1 then Left(failed("SSL_CTX_use_certificate"))
          else if ssl.SSL_CTX_use_PrivateKey(ctx, key) != 1 then Left(failed("SSL_CTX_use_PrivateKey"))
          else if ssl.SSL_CTX_check_private_key(ctx) != 1 then Left(failed("SSL_CTX_check_private_key"))
          else Right(Array(certBytes, keyBytes))
        if cert != null then crypto.X509_free(cert)
        if key != null then crypto.EVP_PKEY_free(key)
        used
      }
    }
  end loadPem

  /** Copies `bytes` into an OpenSSL memory BIO for `use`, and frees it after. `BIO_new_mem_buf` does not copy. */
  private def withBio[A](bytes: Array[Byte])(use: Ptr[Byte] => Either[SslError, A]): Either[SslError, A] =
    val bio = crypto.BIO_new(crypto.BIO_s_mem())
    if bio == null then Left(failed("BIO_new"))
    else
      val result =
        if bytes.nonEmpty && crypto.BIO_write(bio, bytes.at(0), bytes.length) != bytes.length then
          Left(failed("BIO_write"))
        else use(bio)
      val _ = crypto.BIO_free(bio)
      result
  end withBio

  private def fromRsa(n: Array[Byte], e: Array[Byte], d: Option[Array[Byte]]): Ptr[Byte] =
    val rsa = crypto.RSA_new()
    if rsa == null then throw fail("RSA_new")
    val bnN = bin2bn(n)
    val bnE = bin2bn(e)
    val bnD = d.map(bin2bn).getOrElse(null.asInstanceOf[Ptr[Byte]])
    if crypto.RSA_set0_key(rsa, bnN, bnE, bnD) != 1 then
      crypto.BN_free(bnN)
      crypto.BN_free(bnE)
      if bnD != null then crypto.BN_free(bnD)
      crypto.RSA_free(rsa)
      throw fail("RSA_set0_key")
    val pkey = crypto.EVP_PKEY_new()
    if pkey == null then
      crypto.RSA_free(rsa)
      throw fail("EVP_PKEY_new")
    if crypto.EVP_PKEY_assign(pkey, EvpPkeyRsa, rsa) != 1 then
      crypto.EVP_PKEY_free(pkey)
      crypto.RSA_free(rsa)
      throw fail("EVP_PKEY_assign")
    pkey
  end fromRsa

  private def digestSign(pkey: Ptr[Byte], payload: Array[Byte]): Array[Byte] =
    val ctx = crypto.EVP_MD_CTX_new()
    if ctx == null then throw fail("EVP_MD_CTX_new")
    try
      Zone {
        if crypto.EVP_DigestSignInit(ctx, null, crypto.EVP_sha256(), null, pkey) != 1 then
          throw fail("EVP_DigestSignInit")
        val siglen = alloc[CSize]()
        !siglen = 0.toUSize
        val data = if payload.isEmpty then null else payload.at(0)
        if crypto.EVP_DigestSign(ctx, null, siglen, data, payload.length.toUSize) != 1 then
          throw fail("EVP_DigestSign size")
        val sig = new Array[Byte]((!siglen).toInt)
        if crypto.EVP_DigestSign(ctx, sig.at(0), siglen, data, payload.length.toUSize) != 1 then
          throw fail("EVP_DigestSign")
        sig
      }
    finally crypto.EVP_MD_CTX_free(ctx)
    end try
  end digestSign

  private def digestVerify(pkey: Ptr[Byte], payload: Array[Byte], sig: Array[Byte]): Boolean =
    val ctx = crypto.EVP_MD_CTX_new()
    if ctx == null then throw fail("EVP_MD_CTX_new")
    try
      if crypto.EVP_DigestVerifyInit(ctx, null, crypto.EVP_sha256(), null, pkey) != 1 then
        throw fail("EVP_DigestVerifyInit")
      val data = if payload.isEmpty then null else payload.at(0)
      crypto.EVP_DigestVerify(ctx, sig.at(0), sig.length.toUSize, data, payload.length.toUSize) == 1
    catch case _: Exception => false
    finally crypto.EVP_MD_CTX_free(ctx)
  end digestVerify

  private def bnParam(pkey: Ptr[Byte], name: CString): Array[Byte] =
    Zone {
      val bn = alloc[Ptr[Byte]]()
      !bn = null
      if crypto.EVP_PKEY_get_bn_param(pkey, name, bn) != 1 || !bn == null then throw fail("EVP_PKEY_get_bn_param")
      try bn2bin(!bn)
      finally crypto.BN_free(!bn)
    }

  private def bin2bn(bytes: Array[Byte]): Ptr[Byte] =
    val p = crypto.BN_bin2bn(bytes.at(0).asInstanceOf[Ptr[CUnsignedChar]], bytes.length, null)
    if p == null then throw fail("BN_bin2bn")
    p

  private def bn2bin(bn: Ptr[Byte]): Array[Byte] =
    val n = (crypto.BN_num_bits(bn) + 7) / 8
    val a = new Array[Byte](n.max(1))
    val _ = crypto.BN_bn2bin(bn, a.at(0).asInstanceOf[Ptr[CUnsignedChar]])
    a

  /** Reads OpenSSL's error queue now, before a cleanup call can change it. */
  private def failed(function: String): SslError =
    Zone {
      val buf = alloc[CChar](256)
      crypto.ERR_error_string_n(crypto.ERR_get_error(), buf, 256.toUSize)
      SslError.Failed(function, fromCString(buf))
    }

  // The RSA helpers above still throw; T6 rebuilds RSA as a service with typed errors on every platform.
  private def fail(op: String): IOException = failed(op).exception

  @extern
  private object ssl:
    def OPENSSL_init_ssl(opts: CUnsignedLong, settings: Ptr[Byte]): CInt         = extern
    def TLS_server_method(): Ptr[Byte]                                           = extern
    def TLS_client_method(): Ptr[Byte]                                           = extern
    def SSL_CTX_new(method: Ptr[Byte]): Ptr[Byte]                                = extern
    def SSL_CTX_free(ctx: Ptr[Byte]): Unit                                       = extern
    def SSL_CTX_use_certificate(ctx: Ptr[Byte], x: Ptr[Byte]): CInt              = extern
    def SSL_CTX_use_PrivateKey(ctx: Ptr[Byte], pkey: Ptr[Byte]): CInt            = extern
    def SSL_CTX_set_verify(ctx: Ptr[Byte], mode: CInt, cb: Ptr[Byte]): Unit      = extern
    def SSL_CTX_check_private_key(ctx: Ptr[Byte]): CInt                          = extern
    def SSL_new(ctx: Ptr[Byte]): Ptr[Byte]                                       = extern
    def SSL_free(ssl: Ptr[Byte]): Unit                                           = extern
    def SSL_set_fd(ssl: Ptr[Byte], fd: CInt): CInt                               = extern
    @blocking def SSL_accept(ssl: Ptr[Byte]): CInt                               = extern
    @blocking def SSL_connect(ssl: Ptr[Byte]): CInt                              = extern
    @blocking def SSL_read(ssl: Ptr[Byte], buf: Ptr[Byte], num: CInt): CInt      = extern
    @blocking def SSL_write(ssl: Ptr[Byte], buf: Ptr[Byte], num: CInt): CInt     = extern
    def SSL_get_error(ssl: Ptr[Byte], ret: CInt): CInt                           = extern
    def SSL_ctrl(ssl: Ptr[Byte], cmd: CInt, larg: CLong, parg: Ptr[Byte]): CLong = extern
    def SSL_CTX_set_default_verify_paths(ctx: Ptr[Byte]): CInt                   = extern
    def SSL_CTX_get_cert_store(ctx: Ptr[Byte]): Ptr[Byte]                        = extern
    def SSL_set1_host(ssl: Ptr[Byte], hostname: CString): CInt                   = extern
    def SSL_get0_param(ssl: Ptr[Byte]): Ptr[Byte]                                = extern
  end ssl

  private val VerifyPeer = 1

  private val ErrorWantRead         = 2
  private val ErrorWantWrite        = 3
  private val ErrorZeroReturn       = 6
  private val CtrlSetTlsextHostname = 55

  @extern
  private object crypto:
    def SHA256(d: Ptr[CUnsignedChar], n: CSize, md: Ptr[CUnsignedChar]): Ptr[CUnsignedChar] = extern
    def EVP_PKEY_CTX_new_id(id: CInt, e: Ptr[Byte]): Ptr[Byte]                              = extern
    def EVP_PKEY_CTX_free(ctx: Ptr[Byte]): Unit                                             = extern
    def EVP_PKEY_keygen_init(ctx: Ptr[Byte]): CInt                                          = extern
    def EVP_PKEY_CTX_set_rsa_keygen_bits(ctx: Ptr[Byte], bits: CInt): CInt                  = extern
    def EVP_PKEY_keygen(ctx: Ptr[Byte], ppkey: Ptr[Ptr[Byte]]): CInt                        = extern
    def EVP_PKEY_free(pkey: Ptr[Byte]): Unit                                                = extern
    def EVP_PKEY_new(): Ptr[Byte]                                                           = extern
    def EVP_PKEY_assign(pkey: Ptr[Byte], typ: CInt, key: Ptr[Byte]): CInt                   = extern
    def EVP_PKEY_get_bn_param(pkey: Ptr[Byte], keyName: CString, bn: Ptr[Ptr[Byte]]): CInt  = extern
    def EVP_sha256(): Ptr[Byte]                                                             = extern
    def EVP_MD_CTX_new(): Ptr[Byte]                                                         = extern
    def EVP_MD_CTX_free(ctx: Ptr[Byte]): Unit                                               = extern
    def EVP_DigestSignInit(
        ctx: Ptr[Byte],
        pctx: Ptr[Ptr[Byte]],
        typ: Ptr[Byte],
        e: Ptr[Byte],
        pkey: Ptr[Byte],
    ): CInt = extern
    def EVP_DigestSign(
        ctx: Ptr[Byte],
        sig: Ptr[Byte],
        siglen: Ptr[CSize],
        tbs: Ptr[Byte],
        tbslen: CSize,
    ): CInt = extern
    def EVP_DigestVerifyInit(
        ctx: Ptr[Byte],
        pctx: Ptr[Ptr[Byte]],
        typ: Ptr[Byte],
        e: Ptr[Byte],
        pkey: Ptr[Byte],
    ): CInt = extern
    def EVP_DigestVerify(
        ctx: Ptr[Byte],
        sig: Ptr[Byte],
        siglen: CSize,
        tbs: Ptr[Byte],
        tbslen: CSize,
    ): CInt                                                                                               = extern
    def RSA_new(): Ptr[Byte]                                                                              = extern
    def RSA_free(r: Ptr[Byte]): Unit                                                                      = extern
    def RSA_set0_key(r: Ptr[Byte], n: Ptr[Byte], e: Ptr[Byte], d: Ptr[Byte]): CInt                        = extern
    def BN_bin2bn(s: Ptr[CUnsignedChar], len: CInt, ret: Ptr[Byte]): Ptr[Byte]                            = extern
    def BN_bn2bin(a: Ptr[Byte], to: Ptr[CUnsignedChar]): CInt                                             = extern
    def BN_num_bits(a: Ptr[Byte]): CInt                                                                   = extern
    def BN_free(a: Ptr[Byte]): Unit                                                                       = extern
    def BIO_s_mem(): Ptr[Byte]                                                                            = extern
    def BIO_new(method: Ptr[Byte]): Ptr[Byte]                                                             = extern
    def BIO_write(b: Ptr[Byte], data: Ptr[Byte], len: CInt): CInt                                         = extern
    def BIO_free(a: Ptr[Byte]): CInt                                                                      = extern
    def PEM_read_bio_X509(bp: Ptr[Byte], x: Ptr[Ptr[Byte]], cb: Ptr[Byte], u: Ptr[Byte]): Ptr[Byte]       = extern
    def PEM_read_bio_PrivateKey(bp: Ptr[Byte], x: Ptr[Ptr[Byte]], cb: Ptr[Byte], u: Ptr[Byte]): Ptr[Byte] =
      extern
    def X509_free(a: Ptr[Byte]): Unit                                        = extern
    def X509_STORE_add_cert(store: Ptr[Byte], x: Ptr[Byte]): CInt            = extern
    def X509_VERIFY_PARAM_set1_ip_asc(param: Ptr[Byte], ip: CString): CInt   = extern
    def ERR_clear_error(): Unit                                              = extern
    def ERR_get_error(): CUnsignedLong                                       = extern
    def ERR_error_string_n(e: CUnsignedLong, buf: CString, len: CSize): Unit = extern
  end crypto
end Ssl
