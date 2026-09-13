package heddle.server

import heddle.http.{Body, MediaType, Response, Status}
import java.nio.file.{Files as JFiles, Path}
import zio.*
import zio.stream.ZStream

object Files:
  def fromPath(path: Path): Task[Response] =
    ZIO
      .attemptBlocking {
        val p = path.toAbsolutePath.normalize()
        if !JFiles.exists(p) then throw java.nio.file.NoSuchFileException(p.toString)
        if JFiles.isDirectory(p) then throw java.nio.file.AccessDeniedException(p.toString, null, "is a directory")
        val n  = JFiles.size(p)
        val ct = MediaType.fromExtension(extension(p))
        (p, n, ct)
      }
      .map { (p, n, ct) =>
        Response(Status.Ok).withBody(Body.stream(ZStream.fromPath(p), Some(ct), Some(n)))
      }

  private def extension(path: Path): String =
    val name = path.getFileName.toString
    val dot  = name.lastIndexOf('.')
    if dot < 0 then "" else name.substring(dot + 1)
end Files
