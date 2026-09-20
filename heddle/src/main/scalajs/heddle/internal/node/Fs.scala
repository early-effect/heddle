package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait FsStats extends js.Object:
  def isFile(): Boolean      = js.native
  def isDirectory(): Boolean = js.native
  val size: Double           = js.native
  val mtimeMs: Double        = js.native
end FsStats

class ReadStreamOptions(
    var start: js.UndefOr[Double] = js.undefined,
    var end: js.UndefOr[Double] = js.undefined,
) extends js.Object

@js.native
trait ReadStream extends js.Object:
  def on(event: String, listener: js.Function): this.type = js.native
  def destroy(): Unit                                     = js.native

@js.native
@JSImport("node:fs", JSImport.Namespace)
private[heddle] object Fs extends js.Object:
  def readFileSync(path: String): Uint8Array                                 = js.native
  def statSync(path: String): FsStats                                        = js.native
  def writeFileSync(path: String, data: Uint8Array): Unit                    = js.native
  def createReadStream(path: String, options: ReadStreamOptions): ReadStream = js.native
end Fs

@js.native
@JSImport("node:path", JSImport.Namespace)
private[heddle] object NodePath extends js.Object:
  def join(a: String, b: String): String = js.native
  def normalize(p: String): String       = js.native
  def resolve(p: String): String         = js.native
end NodePath
