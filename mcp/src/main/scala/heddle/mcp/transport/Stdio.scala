package heddle.mcp.transport

import heddle.http.header.Headers
import heddle.mcp.protocol.{Engine, Legacy}
import heddle.mcp.protocol.JsonRpc.*
import zio.json.DecoderOps
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.ZIO

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets

object Stdio:
  private enum Era:
    case Modern, Legacy

  def run[R](
      engine: Engine[R],
      in: InputStream = System.in,
      out: OutputStream = System.out,
  ): ZIO[R, Throwable, Unit] =
    ZIO.succeed(new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))).flatMap { reader =>
      def write(msg: Json.Obj): ZIO[Any, Throwable, Unit] =
        ZIO.attemptBlocking {
          val bytes = (msg.toJson + "\n").getBytes(StandardCharsets.UTF_8)
          out.write(bytes)
          out.flush()
        }

      def loop(era: Era): ZIO[R, Throwable, Unit] =
        ZIO.attemptBlocking(reader.readLine()).flatMap {
          case null => ZIO.unit
          case line =>
            dispatch(engine, line, era).flatMap { (next, reply) =>
              reply match
                case None      => loop(next)
                case Some(msg) => write(msg) *> loop(next)
            }
        }

      loop(Era.Modern)
    }

  private def dispatch[R](
      engine: Engine[R],
      line: String,
      era: Era,
  ): ZIO[R, Nothing, (Era, Option[Json.Obj])] =
    if line.isBlank then ZIO.succeed((era, None))
    else
      line.fromJson[Json] match
        case Left(_)     => ZIO.succeed((era, Some(error(Json.Null, ParseError, "Parse error"))))
        case Right(json) =>
          json match
            case msg: Json.Obj =>
              methodOf(msg) match
                case Some("initialize") =>
                  val body =
                    Legacy.initializeResult(
                      parseId(msg),
                      engine.serverName,
                      engine.serverVersion,
                      engine.instructions,
                    )
                  ZIO.succeed((Era.Legacy, Some(body)))
                case Some(method) if isModern(msg, method) =>
                  engine.handle(msg, Headers.empty).map(out => (Era.Modern, out))
                case Some(_) if era == Era.Legacy =>
                  engine.handleCompat(msg, Headers.empty).map(out => (era, out.map(Legacy.stripEnvelope)))
                case _ =>
                  engine.handle(msg, Headers.empty).map(out => (era, out))
            case _ =>
              ZIO.succeed((era, Some(error(Json.Null, ParseError, "Parse error"))))

  private def isModern(msg: Json.Obj, method: String): Boolean =
    method == "server/discover" || protocolVersion(paramsOf(msg)).contains(ProtocolVersion)
end Stdio
