package heddle.error

enum ServerError(val message: String) extends HeddleError:
  case BindFailed(host: String, port: Int, cause: Throwable) extends ServerError(s"Failed to bind $host:$port: $cause")
