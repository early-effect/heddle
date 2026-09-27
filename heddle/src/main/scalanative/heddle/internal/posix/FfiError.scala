package heddle.internal.posix

import heddle.error.HeddleError

/** A failure reported by a C library. The public error cases that carry a `Throwable` cause get `exception`. */
private[heddle] trait FfiError extends HeddleError:
  def exception: java.io.IOException = java.io.IOException(message)
