package heddle.error

import heddle.http.{Response, Status}

/** Why a file could not be served. */
enum FileError(val message: String) extends HeddleError:
  case NotFound(path: String)                     extends FileError(s"no file at $path")
  case IsDirectory(path: String)                  extends FileError(s"$path is a directory")
  case Unreadable(path: String, cause: Throwable) extends FileError(s"cannot read $path: $cause")

  /** A missing file or a directory is 404 to a client; a read failure is the server's, 500. */
  def toResponse: Response =
    this match
      case NotFound(_) | IsDirectory(_) => Response.notFound()
      case Unreadable(_, _)             => Response.empty(Status.InternalServerError)
end FileError
