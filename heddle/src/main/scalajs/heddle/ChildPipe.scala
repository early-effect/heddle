package heddle

import heddle.internal.node.{ChildProcess, ChildProcessHandle, Process, SpawnOptions}
import scala.scalajs.js
import zio.*

/** A child process as a line pipe: its stdout is read, its stdin is written, its stderr is discarded. The scope owns
  * the process: closing it kills the child.
  */
object ChildPipe:
  /** Waits for Node's `spawn` or `error` event, so a missing program is a `SpawnFailed`, not an uncaught error. */
  def spawn(command: ChildCommand): ZIO[Scope, SpawnFailed, LinePipe] =
    ZIO
      .acquireRelease(started(command))(h => ZIO.succeed(h.kill()).unit)
      .map(h => LinePipePlatform.over(h.stdout, h.stdin))

  private def started(command: ChildCommand): IO[SpawnFailed, ChildProcessHandle] =
    ZIO.async[Any, SpawnFailed, ChildProcessHandle] { cb =>
      val options = SpawnOptions(
        stdio = js.Array("pipe", "pipe", "ignore"),
        env = if command.env.isEmpty then js.undefined else merged(command.env),
        cwd = command.cwd.fold[js.UndefOr[String]](js.undefined)(d => d),
      )
      val h = ChildProcess.spawn(command.program, js.Array(command.args*), options)
      h.on("spawn", (() => cb(ZIO.succeed(h))): js.Function0[Unit])
      h.on(
        "error",
        ((e: js.Any) => cb(ZIO.fail(SpawnFailed(command, js.JavaScriptException(e))))): js.Function1[js.Any, Unit],
      )
      ()
    }

  /** Node replaces the whole environment when `env` is given, so the parent's is copied in first. */
  private def merged(extra: Map[String, String]): js.Dictionary[String] =
    val parent = js.Object.keys(Process.env)
    val env    = js.Dictionary.empty[String]
    parent.foreach(k => Process.env(k).foreach(v => env.update(k, v)))
    extra.foreach((k, v) => env.update(k, v))
    env
end ChildPipe
