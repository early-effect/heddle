package heddle.mcp.transport

import heddle.http.header.Headers
import heddle.mcp.protocol.Engine
import zio.json.EncoderOps
import zio.ZIO

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets

object Stdio:
  def run[R](
      engine: Engine[R],
      in: InputStream = System.in,
      out: OutputStream = System.out,
  ): ZIO[R, Throwable, Unit] =
    ZIO.succeed(new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))).flatMap { reader =>
      def loop: ZIO[R, Throwable, Unit] =
        ZIO.attemptBlocking(reader.readLine()).flatMap {
          case null => ZIO.unit
          case line =>
            engine.handleLine(line, Headers.empty).flatMap {
              case None      => loop
              case Some(msg) =>
                ZIO.attemptBlocking {
                  val bytes = (msg.toJson + "\n").getBytes(StandardCharsets.UTF_8)
                  out.write(bytes)
                  out.flush()
                } *> loop
            }
        }
      loop
    }
end Stdio
