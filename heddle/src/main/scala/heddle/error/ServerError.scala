package heddle.error

import heddle.server.OutOfRange
import zio.NonEmptyChunk

enum ServerError(val message: String) extends HeddleError:
  case BindFailed(host: String, port: Int, cause: Throwable) extends ServerError(s"Failed to bind $host:$port: $cause")
  case InvalidConfig(problems: NonEmptyChunk[OutOfRange])
      extends ServerError(s"Invalid server config: ${problems.map(_.message).mkString("; ")}")
