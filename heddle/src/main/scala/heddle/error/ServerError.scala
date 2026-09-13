package heddle.error

enum ServerError(val message: String) extends HeddleError:
  case BindFailed(host: String, port: Int, cause: Throwable) extends ServerError(s"Failed to bind $host:$port: $cause")
  case LoomUnavailable(cause: Throwable)
      extends ServerError(s"heddle.Server requires ZIO's Loom runtime (JDK 21+): $cause")
