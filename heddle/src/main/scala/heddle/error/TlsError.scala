package heddle.error

/** Why TLS material could not become a server `Tls`. */
enum TlsError(val message: String) extends HeddleError:
  case MissingPem(kind: String)   extends TlsError(s"no PEM $kind block")
  case Unusable(cause: Throwable) extends TlsError(s"unusable TLS material: $cause")
