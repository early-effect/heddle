package heddle.server

import heddle.http.{Request, Response}
import zio.*

object Files:
  def fromPath(path: String): Task[Response] =
    ZIO.fail(java.io.IOException(s"no filesystem: $path"))

  def fromDirectory(
      root: String,
      urlPrefix: String,
      request: Request,
      indexHtml: Boolean,
  ): Task[Option[Response]] =
    ZIO.succeed {
      val _ = (root, indexHtml)
      SafePath.remainder(urlPrefix, request.path).flatMap(_ => None)
    }

  def fromResource(name: String, request: Request): Task[Option[Response]] =
    ZIO.succeed {
      val _ = request
      SafePath.resolveUnder("", name.stripPrefix("/")).flatMap(_ => None)
    }
end Files
