package heddle

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters.*
import scala.util.Try

object Resources:
  def pid: Long = ProcessHandle.current().pid()

  def heapUsed: Long =
    val bean = ManagementFactory.getMemoryMXBean
    bean.getHeapMemoryUsage.getUsed

  /** Best-effort heap settle for leak tests. `System.gc` is a hint, not an effect. */
  def gc(): Unit =
    System.gc()
    Thread.sleep(50)
    System.gc()
    Thread.sleep(50)

  def platformThreads: Int =
    Thread.getAllStackTraces.keySet.asScala.count(t => t.isAlive && !t.isVirtual)

  def establishedTcp: Option[Int] = lsofEstablished("-iTCP")

  /** Same-process ESTABLISHED sockets on `port` only. Process-wide counts race the parallel live-server suite. */
  def establishedTcpOn(port: Int): Option[Int] = lsofEstablished(s"-iTCP:$port")

  /** Open file descriptors for this process. Linux `/proc/<pid>/fd`, else `/dev/fd`. */
  def openFiles: Option[Int] =
    Try {
      val linux = java.nio.file.Path.of(s"/proc/$pid/fd")
      val dir   = if java.nio.file.Files.isDirectory(linux) then linux else java.nio.file.Path.of("/dev/fd")
      if !java.nio.file.Files.isDirectory(dir) then None
      else Some(java.nio.file.Files.list(dir).count().toInt)
    }.toOption.flatten

  private def lsofEstablished(filter: String): Option[Int] =
    val os = sys.props.getOrElse("os.name", "").toLowerCase
    if os.contains("win") then None
    else
      Try {
        val pb   = ProcessBuilder("lsof", "-nP", "-a", "-p", pid.toString, filter, "-sTCP:ESTABLISHED")
        val proc = pb.start()
        val out  = String(proc.getInputStream.readAllBytes())
        proc.waitFor()
        out.linesIterator.drop(1).count(_.nonEmpty)
      }.toOption
  end lsofEstablished
end Resources
