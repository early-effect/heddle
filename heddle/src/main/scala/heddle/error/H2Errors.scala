package heddle.error

/** Why a header block did not decode: an HTTP/2 COMPRESSION_ERROR (RFC 9113 §4.3). */
enum HpackError(val message: String) extends HeddleError:
  case BadIndex(index: Int) extends HpackError(s"index $index is in neither table")
  case Truncated            extends HpackError("the block ends inside a field")
  case BadHuffman           extends HpackError("a Huffman string is malformed")
  case IntegerOverflow      extends HpackError("an integer does not fit 32 bits")
  case TableTooLarge(size: Int, limit: Int)
      extends HpackError(s"a table size update to $size is over the $limit this endpoint allows")
  case LateSizeUpdate extends HpackError("a table size update follows a header field")
  case ListTooLarge(size: Long, limit: Long)
      extends HpackError(s"the header list is $size octets, over the $limit this endpoint allows")
end HpackError

/** A peer broke an HTTP/2 rule that is not about framing bytes (RFC 9113). */
enum H2Violation(val message: String, val code: Int):
  case ContinuationExpected(stream: Int, frameType: Int)
      extends H2Violation(
        s"stream $stream's header block is open, and a frame of type $frameType came before its CONTINUATION",
        0x1,
      )
  case UnexpectedContinuation(stream: Int)
      extends H2Violation(s"a CONTINUATION for stream $stream follows no open header block", 0x1)
  case StreamIdRegressed(stream: Int, last: Int) extends H2Violation(s"stream $stream opened after stream $last", 0x1)
  case EvenStreamId(stream: Int)                 extends H2Violation(s"a client opened even stream $stream", 0x1)
  case BadSetting(setting: Int, value: Long)
      extends H2Violation(s"SETTINGS $setting cannot be $value", if setting == 4 then 0x3 else 0x1)
  case WindowOverflow(stream: Int)
      extends H2Violation(s"a WINDOW_UPDATE pushes stream $stream's window past 2^31-1", 0x3)
  case ZeroIncrement extends H2Violation("a connection WINDOW_UPDATE of 0", 0x1)
end H2Violation
