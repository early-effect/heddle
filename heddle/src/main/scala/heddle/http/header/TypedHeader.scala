package heddle.http.header

import heddle.error.ParamError
import heddle.http.MediaType

/** Parse-on-read typed header. Decode failure is `Left`; missing fields stay `None`. */
trait TypedHeader[A]:
  def name: HeaderName
  def decode(raw: String): Either[ParamError, A]
  def encode(a: A): String

object TypedHeader:
  given TypedHeader[MediaType] with
    def name: HeaderName                                   = HeaderName.ContentType
    def decode(raw: String): Either[ParamError, MediaType] =
      MediaType.parse(raw).toRight(ParamError.Malformed(raw, "a media type"))
    def encode(a: MediaType): String = a.render
