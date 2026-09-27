package heddle.brotli

final class BrotliException(msg: String) extends IllegalArgumentException(msg)

/** Decoded output would pass the caller's limit. */
private[brotli] final class OverLimit extends RuntimeException("brotli output over limit", null, false, false)
