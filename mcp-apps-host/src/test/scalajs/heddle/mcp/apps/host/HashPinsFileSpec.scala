package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import zio.*
import zio.test.*

object HashPinsFileSpec extends ZIOSpecDefault:
  private val key   = PinKey(ServerName("counter"), UiUri("ui://early-effect/counter"))
  private val bytes = Digest.of("<html></html>")

  def spec = suite("PinFiles")(
    test("a pin written through node is what the next layer reads"):
      val dir = NodePath.join(NodeOs.tmpdir(), "heddle-pins-" + java.lang.System.nanoTime().toString)
      ZIO.acquireRelease(ZIO.attempt(NodeFs.mkdirSync(dir, MkdirRecursive())).as(dir))(deleteDir).flatMap { root =>
        val path = NodePath.join(root, "pins.json")
        for
          first <- ZIO.serviceWithZIO[HashPins](_.check(key, bytes)).provide(HashPins.durable(PinFiles.at(path)))
          again <- ZIO.serviceWithZIO[HashPins](_.check(key, bytes)).provide(HashPins.durable(PinFiles.at(path)))
        yield assertTrue(first == PinCheck.FirstSeen, again == PinCheck.Same)
      }
  )

  private def deleteDir(path: String): UIO[Unit] =
    ZIO.attempt(NodeFs.rmSync(path, RmRecursive())).ignore

  @js.native
  @JSImport("node:fs", JSImport.Namespace)
  private object NodeFs extends js.Object:
    def mkdirSync(path: String, options: js.Object): Unit = js.native
    def rmSync(path: String, options: js.Object): Unit    = js.native

  @js.native
  @JSImport("node:os", JSImport.Namespace)
  private object NodeOs extends js.Object:
    def tmpdir(): String = js.native

  @js.native
  @JSImport("node:path", JSImport.Namespace)
  private object NodePath extends js.Object:
    def join(a: String, b: String): String = js.native

  private class MkdirRecursive extends js.Object:
    val recursive: Boolean = true

  private class RmRecursive extends js.Object:
    val recursive: Boolean = true
    val force: Boolean     = true
end HashPinsFileSpec
