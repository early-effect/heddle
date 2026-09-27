package heddle.internal.posix

/** Which readiness a parked fiber waits for. */
private[heddle] enum Interest:
  case Read, Write

/** What one non-blocking read or write did. TLS can block a read on writability and a write on readability. */
private[heddle] enum Transfer:
  case Moved(bytes: Int)
  case Eof
  case Blocked(on: Interest)
