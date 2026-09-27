package heddle.brotli

/** Why a brotli stream did not decode (RFC 7932). */
private[brotli] enum BrotliError(val message: String):
  case Truncated                     extends BrotliError("the stream ends early")
  case CorruptPadding                extends BrotliError("padding bits are not zero")
  case Unaligned                     extends BrotliError("uncompressed bytes do not start on a byte boundary")
  case ReservedBit                   extends BrotliError("a reserved bit is set")
  case ExuberantNibble               extends BrotliError("a length has a needless zero nibble")
  case DuplicateSymbol               extends BrotliError("a simple prefix code repeats a symbol")
  case IncompleteCode                extends BrotliError("a prefix code does not fill its code space")
  case RepeatOverflow                extends BrotliError("a repeated code length runs past the alphabet")
  case BadContextMap                 extends BrotliError("a context map names a tree that does not exist")
  case BadDistance                   extends BrotliError("a distance is not positive")
  case BadReference                  extends BrotliError("a backward reference points outside the window or dictionary")
  case OverLimit                     extends BrotliError("the output would pass its limit")
  case MissingResource(name: String) extends BrotliError(s"the bundled $name did not load")
end BrotliError
