package heddle

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import zio.*

/** A child process as a line pipe: its stdout is read, its stdin is written, its stderr is discarded. The scope owns
  * the process: closing it (or interrupting a blocked read) stops the child.
  */
object ChildPipe:
  def spawn(command: ChildCommand): ZIO[Scope, SpawnFailed, LinePipe] =
    ZIO
      .acquireRelease(ZIO.attemptBlocking(start(command)).mapError(SpawnFailed(command, _)))(stop)
      .map(ProcessPipe(_))

  private def start(command: ChildCommand): Process =
    val pb = ProcessBuilder((command.program +: command.args).asJava)
    command.cwd.foreach(d => pb.directory(java.io.File(d)))
    pb.environment().putAll(command.env.asJava)
    pb.redirectError(ProcessBuilder.Redirect.DISCARD)
    pb.start()

  private def stop(p: Process): UIO[Unit] =
    ZIO
      .attemptBlocking {
        p.destroy()
        if !p.waitFor(2, TimeUnit.SECONDS) then p.destroyForcibly()
      }
      .ignore
      .unit

  private final class ProcessPipe(p: Process) extends LinePipe:
    private val reader = BufferedReader(InputStreamReader(p.getInputStream, StandardCharsets.UTF_8))

    /** A blocking read ignores interruption; stopping the child ends it. */
    def readLine: IO[Throwable, Option[String]] =
      ZIO.attemptBlockingCancelable(Option(reader.readLine()))(ZIO.succeed(p.destroy()))

    def writeLine(line: String): Task[Unit] =
      ZIO.attemptBlocking {
        val out = p.getOutputStream
        out.write((line + "\n").getBytes(StandardCharsets.UTF_8))
        out.flush()
      }
  end ProcessPipe
end ChildPipe
