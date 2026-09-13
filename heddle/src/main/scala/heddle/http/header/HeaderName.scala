package heddle.http.header

import heddle.internal.Ascii

/** Standard field name. Catalog vals are interned; unknown names are not. */
final class HeaderName private[header] (val render: String, private[header] val lower: String):
  override def equals(other: Any): Boolean =
    other match
      case n: HeaderName => (this eq n) || lower == n.lower
      case _             => false

  override def hashCode: Int = lower.hashCode

  override def toString: String = render

  def http2: String = lower
end HeaderName

object HeaderName:
  private val MaxLen                                         = 64
  private val byLength: Array[Array[HeaderName]]             = Array.fill(MaxLen + 1)(Array.empty[HeaderName])
  private val byLower: java.util.HashMap[String, HeaderName] = java.util.HashMap()

  private def interned(render: String): HeaderName =
    val n = new HeaderName(render, asciiLower(render))
    val i = render.length
    if i <= MaxLen then byLength(i) = byLength(i) :+ n
    byLower.put(n.lower, n)
    n

  val ContentEncoding: HeaderName               = interned("Content-Encoding")
  val ContentLanguage: HeaderName               = interned("Content-Language")
  val ContentLength: HeaderName                 = interned("Content-Length")
  val ContentLocation: HeaderName               = interned("Content-Location")
  val ContentRange: HeaderName                  = interned("Content-Range")
  val ContentType: HeaderName                   = interned("Content-Type")
  val Trailer: HeaderName                       = interned("Trailer")
  val TransferEncoding: HeaderName              = interned("Transfer-Encoding")
  val TE: HeaderName                            = interned("TE")
  val Accept: HeaderName                        = interned("Accept")
  val AcceptCharset: HeaderName                 = interned("Accept-Charset")
  val AcceptEncoding: HeaderName                = interned("Accept-Encoding")
  val AcceptLanguage: HeaderName                = interned("Accept-Language")
  val Authorization: HeaderName                 = interned("Authorization")
  val Expect: HeaderName                        = interned("Expect")
  val From: HeaderName                          = interned("From")
  val Host: HeaderName                          = interned("Host")
  val IfMatch: HeaderName                       = interned("If-Match")
  val IfModifiedSince: HeaderName               = interned("If-Modified-Since")
  val IfNoneMatch: HeaderName                   = interned("If-None-Match")
  val IfRange: HeaderName                       = interned("If-Range")
  val IfUnmodifiedSince: HeaderName             = interned("If-Unmodified-Since")
  val MaxForwards: HeaderName                   = interned("Max-Forwards")
  val ProxyAuthorization: HeaderName            = interned("Proxy-Authorization")
  val Range: HeaderName                         = interned("Range")
  val Referer: HeaderName                       = interned("Referer")
  val UserAgent: HeaderName                     = interned("User-Agent")
  val AcceptRanges: HeaderName                  = interned("Accept-Ranges")
  val Age: HeaderName                           = interned("Age")
  val Allow: HeaderName                         = interned("Allow")
  val AuthenticationInfo: HeaderName            = interned("Authentication-Info")
  val Date: HeaderName                          = interned("Date")
  val ETag: HeaderName                          = interned("ETag")
  val Location: HeaderName                      = interned("Location")
  val ProxyAuthenticate: HeaderName             = interned("Proxy-Authenticate")
  val ProxyAuthenticationInfo: HeaderName       = interned("Proxy-Authentication-Info")
  val RetryAfter: HeaderName                    = interned("Retry-After")
  val Server: HeaderName                        = interned("Server")
  val Vary: HeaderName                          = interned("Vary")
  val WwwAuthenticate: HeaderName               = interned("WWW-Authenticate")
  val Connection: HeaderName                    = interned("Connection")
  val KeepAlive: HeaderName                     = interned("Keep-Alive")
  val Upgrade: HeaderName                       = interned("Upgrade")
  val Via: HeaderName                           = interned("Via")
  val Warning: HeaderName                       = interned("Warning")
  val CacheControl: HeaderName                  = interned("Cache-Control")
  val Expires: HeaderName                       = interned("Expires")
  val Pragma: HeaderName                        = interned("Pragma")
  val LastModified: HeaderName                  = interned("Last-Modified")
  val Origin: HeaderName                        = interned("Origin")
  val AccessControlAllowOrigin: HeaderName      = interned("Access-Control-Allow-Origin")
  val AccessControlAllowMethods: HeaderName     = interned("Access-Control-Allow-Methods")
  val AccessControlAllowHeaders: HeaderName     = interned("Access-Control-Allow-Headers")
  val AccessControlAllowCredentials: HeaderName = interned("Access-Control-Allow-Credentials")
  val AccessControlExposeHeaders: HeaderName    = interned("Access-Control-Expose-Headers")
  val AccessControlMaxAge: HeaderName           = interned("Access-Control-Max-Age")
  val AccessControlRequestMethod: HeaderName    = interned("Access-Control-Request-Method")
  val AccessControlRequestHeaders: HeaderName   = interned("Access-Control-Request-Headers")
  val SecWebSocketKey: HeaderName               = interned("Sec-WebSocket-Key")
  val SecWebSocketAccept: HeaderName            = interned("Sec-WebSocket-Accept")
  val SecWebSocketVersion: HeaderName           = interned("Sec-WebSocket-Version")
  val SecWebSocketProtocol: HeaderName          = interned("Sec-WebSocket-Protocol")
  val SecWebSocketExtensions: HeaderName        = interned("Sec-WebSocket-Extensions")
  val Cookie: HeaderName                        = interned("Cookie")
  val SetCookie: HeaderName                     = interned("Set-Cookie")
  val LastEventId: HeaderName                   = interned("Last-Event-ID")
  val Forwarded: HeaderName                     = interned("Forwarded")
  val XForwardedFor: HeaderName                 = interned("X-Forwarded-For")
  val XForwardedProto: HeaderName               = interned("X-Forwarded-Proto")
  val XForwardedHost: HeaderName                = interned("X-Forwarded-Host")
  val XRequestId: HeaderName                    = interned("X-Request-Id")
  val ContentDisposition: HeaderName            = interned("Content-Disposition")
  val Link: HeaderName                          = interned("Link")
  val Refresh: HeaderName                       = interned("Refresh")
  val StrictTransportSecurity: HeaderName       = interned("Strict-Transport-Security")
  val AltSvc: HeaderName                        = interned("Alt-Svc")
  val AcceptPatch: HeaderName                   = interned("Accept-Patch")
  val AcceptPost: HeaderName                    = interned("Accept-Post")
  val ContentSecurityPolicy: HeaderName         = interned("Content-Security-Policy")
  val XContentTypeOptions: HeaderName           = interned("X-Content-Type-Options")
  val XFrameOptions: HeaderName                 = interned("X-Frame-Options")
  val ReferrerPolicy: HeaderName                = interned("Referrer-Policy")
  val CrossOriginOpenerPolicy: HeaderName       = interned("Cross-Origin-Opener-Policy")
  val CrossOriginResourcePolicy: HeaderName     = interned("Cross-Origin-Resource-Policy")
  val CrossOriginEmbedderPolicy: HeaderName     = interned("Cross-Origin-Embedder-Policy")

  /** Catalog hit by ASCII-lowercase, or a fresh non-interned name. `:protocol` is always ad-hoc. */
  def apply(raw: String): HeaderName =
    val hit = byLower.get(asciiLower(raw))
    if hit != null then hit else new HeaderName(raw, asciiLower(raw))

  /** Length-bucket intern from HTTP/1.1 header bytes. Unknown names are not added to the catalog. */
  private[heddle] def intern(raw: Array[Byte], from: Int, until: Int): HeaderName =
    val n = until - from
    if n <= 0 || n > MaxLen then
      new HeaderName(Ascii.string(raw, from, until), asciiLower(Ascii.string(raw, from, until)))
    else
      val bucket = byLength(n)
      var i      = 0
      while i < bucket.length do
        if Ascii.eqIgnoreCase(raw, from, until, bucket(i).render) then return bucket(i)
        i += 1
      val render = Ascii.string(raw, from, until)
      new HeaderName(render, asciiLower(render))

  private def asciiLower(s: String): String =
    val arr = s.toCharArray
    var i   = 0
    while i < arr.length do
      val c = arr(i)
      if c >= 'A' && c <= 'Z' then arr(i) = (c + 32).toChar
      i += 1
    String(arr)
end HeaderName
