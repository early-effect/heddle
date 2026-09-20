package heddle

/** JVM HTTP scheduler slot. JS and Native ignore this. Bind must not fail if Loom cannot be enabled. */
enum JvmScheduler:
  case Loom
  case Default
