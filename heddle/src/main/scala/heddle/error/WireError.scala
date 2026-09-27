package heddle.error

import heddle.http.ContentEncoding

/** Bytes that break HTTP/1.1 or HTTP/2 framing, or a content coding that does not decode. */
enum WireError(val message: String) extends HeddleError:
  case BadRequestLine(line: String)  extends WireError(s"not a request line: $line")
  case UnknownMethod(token: String)  extends WireError(s"unknown method: $token")
  case BadStatusLine(line: String)   extends WireError(s"not a status line: $line")
  case BadHeaderLine                 extends WireError("a header line has no name before its colon")
  case TruncatedHeaders              extends WireError("the header block ends before its blank line")
  case BadContentLength(raw: String) extends WireError(s"not a Content-Length: $raw")
  case TruncatedBody                 extends WireError("the body ends before its declared length")
  case ChunkLineTooLong              extends WireError("a chunk-size line is too long")
  case BadChunkSize(token: String)   extends WireError(s"not a chunk size: $token")
  case TruncatedChunk                extends WireError("a chunked body ends mid-chunk")
  case MissingChunkCrlf              extends WireError("a chunk is not followed by CRLF")
  case NoH2Preface                   extends WireError("ALPN chose h2, and the client did not send the h2 preface")
  case TruncatedFrame                extends WireError("an HTTP/2 frame ends early")
  case FrameTooLarge(length: Int, max: Int)
      extends WireError(s"an HTTP/2 frame of $length bytes is over the $max limit")
  case FrameSize(frameType: Int, length: Int)
      extends WireError(s"an HTTP/2 frame of type $frameType cannot be $length bytes (RFC 9113 §4.2)")
  case BadHeaderBlock(reason: HpackError)
      extends WireError(s"an HTTP/2 header block does not decode: ${reason.message}")
  case H2Protocol(violation: H2Violation) extends WireError(violation.message)
  case BadPadding(padding: Int, payload: Int)
      extends WireError(s"$padding bytes of padding do not fit a $payload-byte HTTP/2 payload (RFC 9113 §6.1)")
  case Undecodable(coding: ContentEncoding, detail: String)
      extends WireError(s"the ${coding.token} body does not decode: $detail")

  /** The RFC 9113 §7 error code a GOAWAY answers this with. */
  def h2Code: Int =
    this match
      case BadHeaderBlock(_)                     => 0x9
      case H2Protocol(v)                         => v.code
      case FrameTooLarge(_, _) | FrameSize(_, _) => 0x6
      case _                                     => 0x1
end WireError
