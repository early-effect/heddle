package heddle.mcp.apps.host

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import zio.{IO, ZIO}

/** A pin file through Node. The write lands beside the file, then replaces it. */
object PinFiles:
  def at(path: String): PinFile = new PinFile:
    def read: IO[PinFileError, Option[String]] =
      ZIO
        .attempt {
          if NodeFs.existsSync(path) then Some(NodeFs.readFileSync(path, "utf8"))
          else None
        }
        .mapError(e => PinFileError.Unreadable(e.getClass.getSimpleName))

    def write(text: String): IO[PinFileError, Unit] =
      ZIO
        .attempt {
          NodeFs.mkdirSync(NodePath.dirname(path), MkdirRecursive())
          val tmp = path + ".tmp"
          NodeFs.writeFileSync(tmp, text)
          NodeFs.renameSync(tmp, path)
        }
        .mapError(e => PinFileError.Unwritable(e.getClass.getSimpleName))

  @js.native
  @JSImport("node:fs", JSImport.Namespace)
  private object NodeFs extends js.Object:
    def existsSync(path: String): Boolean                    = js.native
    def readFileSync(path: String, encoding: String): String = js.native
    def writeFileSync(path: String, data: String): Unit      = js.native
    def mkdirSync(path: String, options: js.Object): Unit    = js.native
    def renameSync(from: String, to: String): Unit           = js.native

  @js.native
  @JSImport("node:path", JSImport.Namespace)
  private object NodePath extends js.Object:
    def dirname(path: String): String = js.native

  private class MkdirRecursive extends js.Object:
    val recursive: Boolean = true
end PinFiles
