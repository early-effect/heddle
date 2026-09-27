package heddle

import java.net.Socket
import java.nio.charset.StandardCharsets
import zio.*
import zio.test.*

object MalformedTargetSpec extends ZIOSpecDefault:
  private val routes = Routes(Method.GET / "ok" -> Handler.text("ok"))

  private def raw(port: Int, head: String): Task[String] =
    ZIO.attemptBlocking {
      val s = Socket("127.0.0.1", port)
      try
        s.setSoTimeout(5000)
        s.getOutputStream.write(head.getBytes(StandardCharsets.US_ASCII))
        s.getOutputStream.flush()
        val buf = new Array[Byte](512)
        val n   = s.getInputStream.read(buf)
        if n <= 0 then "" else String(buf, 0, n, StandardCharsets.US_ASCII)
      finally s.close()
    }

  def spec = suite("Malformed request target")(
    test("a bad percent-escape still gets an HTTP answer") {
      ZIO
        .scoped {
          Server.install(routes, LiveServer.local).flatMap { server =>
            server.port.flatMap(port => raw(port, "GET /%zz HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"))
          }
        }
        .map(reply => assertTrue(reply.startsWith("HTTP/1.1 404")))
    }
  )
end MalformedTargetSpec
