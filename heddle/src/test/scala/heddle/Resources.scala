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

  def establishedTcp: Option[Int] =
    val os = sys.props.getOrElse("os.name", "").toLowerCase
    if os.contains("win") then None
    else
      Try {
        val pb   = ProcessBuilder("lsof", "-nP", "-a", "-p", pid.toString, "-iTCP", "-sTCP:ESTABLISHED")
        val proc = pb.start()
        val out  = String(proc.getInputStream.readAllBytes())
        proc.waitFor()
        out.linesIterator.drop(1).count(_.nonEmpty)
      }.toOption
  end establishedTcp
end Resources
