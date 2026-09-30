package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import java.nio.file.{Files, Path}
import zio.*
import zio.test.*

object HashPinsFileSpec extends ZIOSpecDefault:
  private val key   = PinKey(ServerName("counter"), UiUri("ui://early-effect/counter"))
  private val bytes = Digest.of("<html></html>")

  def spec = suite("PinFiles")(
    test("a pin written to disk is what the next layer reads"):
      ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("heddle-pins")))(deleteTree).flatMap { dir =>
        val path = dir.resolve("pins.json")
        for
          first <- ZIO.serviceWithZIO[HashPins](_.check(key, bytes)).provide(HashPins.durable(PinFiles.at(path)))
          again <- ZIO.serviceWithZIO[HashPins](_.check(key, bytes)).provide(HashPins.durable(PinFiles.at(path)))
        yield assertTrue(first == PinCheck.FirstSeen, again == PinCheck.Same)
      }
  )

  private def deleteTree(path: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.isDirectory(path) then
        val listed = Files.list(path)
        try listed.forEach(deleteTreeSync)
        finally listed.close()
      Files.deleteIfExists(path)
    }.ignore

  private def deleteTreeSync(path: Path): Unit =
    if Files.isDirectory(path) then
      val listed = Files.list(path)
      try listed.forEach(deleteTreeSync)
      finally listed.close()
    Files.deleteIfExists(path)
end HashPinsFileSpec
