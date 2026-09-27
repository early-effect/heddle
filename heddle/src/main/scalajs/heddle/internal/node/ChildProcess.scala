package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
private[heddle] trait ChildProcessHandle extends js.Object:
  val stdin: ProcessStream                                = js.native
  val stdout: ProcessStream                               = js.native
  def on(event: String, listener: js.Function): this.type = js.native
  def kill(): Boolean                                     = js.native

/** `stdio` is `[stdin, stdout, stderr]`: `"pipe"` or `"ignore"`. */
private[heddle] final class SpawnOptions(
    val stdio: js.Array[String],
    val env: js.UndefOr[js.Dictionary[String]],
    val cwd: js.UndefOr[String],
) extends js.Object

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private[heddle] object ChildProcess extends js.Object:
  def spawn(command: String, args: js.Array[String], options: SpawnOptions): ChildProcessHandle = js.native
