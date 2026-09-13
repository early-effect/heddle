package heddle.http

import java.nio.charset.StandardCharsets
import zio.Chunk

final case class Status(code: Int, text: String):
  def isSuccess: Boolean       = code / 100 == 2
  def isClientError: Boolean   = code / 100 == 4
  def isServerError: Boolean   = code / 100 == 5
  val render: String           = s"$code $text"
  val renderBytes: Chunk[Byte] = Chunk.fromArray(render.getBytes(StandardCharsets.US_ASCII))

object Status:
  val Continue                      = Status(100, "Continue")
  val SwitchingProtocols            = Status(101, "Switching Protocols")
  val EarlyHints                    = Status(103, "Early Hints")
  val Ok                            = Status(200, "OK")
  val Created                       = Status(201, "Created")
  val Accepted                      = Status(202, "Accepted")
  val NonAuthoritativeInformation   = Status(203, "Non-Authoritative Information")
  val NoContent                     = Status(204, "No Content")
  val ResetContent                  = Status(205, "Reset Content")
  val PartialContent                = Status(206, "Partial Content")
  val MultipleChoices               = Status(300, "Multiple Choices")
  val MovedPermanently              = Status(301, "Moved Permanently")
  val Found                         = Status(302, "Found")
  val SeeOther                      = Status(303, "See Other")
  val NotModified                   = Status(304, "Not Modified")
  val UseProxy                      = Status(305, "Use Proxy")
  val TemporaryRedirect             = Status(307, "Temporary Redirect")
  val PermanentRedirect             = Status(308, "Permanent Redirect")
  val BadRequest                    = Status(400, "Bad Request")
  val Unauthorized                  = Status(401, "Unauthorized")
  val PaymentRequired               = Status(402, "Payment Required")
  val Forbidden                     = Status(403, "Forbidden")
  val NotFound                      = Status(404, "Not Found")
  val MethodNotAllowed              = Status(405, "Method Not Allowed")
  val NotAcceptable                 = Status(406, "Not Acceptable")
  val ProxyAuthenticationRequired   = Status(407, "Proxy Authentication Required")
  val RequestTimeout                = Status(408, "Request Timeout")
  val Conflict                      = Status(409, "Conflict")
  val Gone                          = Status(410, "Gone")
  val LengthRequired                = Status(411, "Length Required")
  val PreconditionFailed            = Status(412, "Precondition Failed")
  val ContentTooLarge               = Status(413, "Content Too Large")
  val UriTooLong                    = Status(414, "URI Too Long")
  val UnsupportedMediaType          = Status(415, "Unsupported Media Type")
  val RangeNotSatisfiable           = Status(416, "Range Not Satisfiable")
  val ExpectationFailed             = Status(417, "Expectation Failed")
  val MisdirectedRequest            = Status(421, "Misdirected Request")
  val UnprocessableContent          = Status(422, "Unprocessable Content")
  val TooEarly                      = Status(425, "Too Early")
  val UpgradeRequired               = Status(426, "Upgrade Required")
  val PreconditionRequired          = Status(428, "Precondition Required")
  val TooManyRequests               = Status(429, "Too Many Requests")
  val RequestHeaderFieldsTooLarge   = Status(431, "Request Header Fields Too Large")
  val UnavailableForLegalReasons    = Status(451, "Unavailable For Legal Reasons")
  val InternalServerError           = Status(500, "Internal Server Error")
  val NotImplemented                = Status(501, "Not Implemented")
  val BadGateway                    = Status(502, "Bad Gateway")
  val ServiceUnavailable            = Status(503, "Service Unavailable")
  val GatewayTimeout                = Status(504, "Gateway Timeout")
  val HttpVersionNotSupported       = Status(505, "HTTP Version Not Supported")
  val VariantAlsoNegotiates         = Status(506, "Variant Also Negotiates")
  val InsufficientStorage           = Status(507, "Insufficient Storage")
  val LoopDetected                  = Status(508, "Loop Detected")
  val NetworkAuthenticationRequired = Status(511, "Network Authentication Required")

  private val known: Map[Int, Status] =
    List(
      Continue,
      SwitchingProtocols,
      EarlyHints,
      Ok,
      Created,
      Accepted,
      NonAuthoritativeInformation,
      NoContent,
      ResetContent,
      PartialContent,
      MultipleChoices,
      MovedPermanently,
      Found,
      SeeOther,
      NotModified,
      UseProxy,
      TemporaryRedirect,
      PermanentRedirect,
      BadRequest,
      Unauthorized,
      PaymentRequired,
      Forbidden,
      NotFound,
      MethodNotAllowed,
      NotAcceptable,
      ProxyAuthenticationRequired,
      RequestTimeout,
      Conflict,
      Gone,
      LengthRequired,
      PreconditionFailed,
      ContentTooLarge,
      UriTooLong,
      UnsupportedMediaType,
      RangeNotSatisfiable,
      ExpectationFailed,
      MisdirectedRequest,
      UnprocessableContent,
      TooEarly,
      UpgradeRequired,
      PreconditionRequired,
      TooManyRequests,
      RequestHeaderFieldsTooLarge,
      UnavailableForLegalReasons,
      InternalServerError,
      NotImplemented,
      BadGateway,
      ServiceUnavailable,
      GatewayTimeout,
      HttpVersionNotSupported,
      VariantAlsoNegotiates,
      InsufficientStorage,
      LoopDetected,
      NetworkAuthenticationRequired,
    ).map(s => s.code -> s).toMap

  /** Catalog status for a known code, otherwise `Status(code, "Unknown")`. */
  def fromCode(code: Int): Status =
    known.getOrElse(code, Status(code, "Unknown"))
end Status
