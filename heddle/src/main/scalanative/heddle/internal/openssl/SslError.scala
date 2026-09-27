package heddle.internal.openssl

import heddle.internal.posix.FfiError

/** An OpenSSL call that failed, with the reason its error queue gave. */
private[heddle] enum SslError(val message: String) extends FfiError:
  case Failed(function: String, reason: String) extends SslError(s"$function: $reason")
  case PeerClosed(function: String)             extends SslError(s"$function: the peer closed the connection")
  case NoTrustedCertificates                    extends SslError("the trust PEM holds no certificates")
  case ClosedSession                            extends SslError("the TLS session is already closed")
