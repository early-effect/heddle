package heddle.mcp

import heddle.*
import heddle.mcp.client.{McpClient, McpError, McpSession}
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** One server, both transports: a later call finishes first, a tool notification precedes its result, and cancelling
  * one call leaves the other running.
  */
object SessionTransportSpec extends ZIOSpecDefault:
  final case class Arg(name: String) derives Schema, JsonCodec

  private val settings = McpClient.Settings(Implementation("spec", "0"))

  private val note: Message.Notification =
    Message.Notification("notifications/progress", Json.Obj("token" -> Json.Str("tick")))

  private val hold = ClientRequest.CallTool(ToolName("hold"), Json.Obj("name" -> Json.Str("ada")))
  private val pass = ClientRequest.CallTool(ToolName("pass"), Json.Obj("name" -> Json.Str("bea")))
  private val tick = ClientRequest.CallTool(ToolName("tick"), Json.Obj("name" -> Json.Str("ada")))

  private def said(result: Json.Obj, needle: String): Boolean =
    result.as[CallToolResult] match
      case Right(got) =>
        got.content.exists {
          case ContentBlock.Text(text, _) => text == needle
          case _                          => false
        }
      case Left(_) => false

  private def pipe(mcp: Mcp[Any]): ZIO[Scope, McpError, McpSession] =
    LinePipe.connected.flatMap { (client, remote) =>
      mcp.stdio(remote).forkScoped *> McpClient.pipe(client, settings)
    }

  private def http(mcp: Mcp[Any]): ZIO[Scope, McpError, McpSession] =
    McpClient.http("http://session.test/mcp", settings).provideSome[Scope](Client.inMemory(mcp.routes))

  private def holdAndPass(
      hold: Arg => UIO[String],
      pass: Arg => UIO[String],
  ): ZIO[Any, NonEmptyChunk[McpBuildError], Mcp[Any]] =
    ZIO.fromEither(
      Mcp.from(Api("session", "0")).flatMap(_.tool[Arg]("hold")(hold)).flatMap(_.tool[Arg]("pass")(pass))
    )

  def spec = suite("a session stays open on both transports")(
    transport("stdio", pipe),
    transport("HTTP", http),
  ) @@ TestAspect.timeout(60.seconds)

  private def transport(name: String, connect: Mcp[Any] => ZIO[Scope, McpError, McpSession]) =
    suite(name)(
      test("a later call finishes while the earlier tool is still waiting"):
        ZIO.scoped {
          for
            started <- Promise.make[Nothing, Unit]
            gate    <- Promise.make[Nothing, Unit]
            mcp     <- holdAndPass(
              a => started.succeed(()) *> gate.await *> ZIO.succeed(a.name),
              a => ZIO.succeed(a.name),
            )
            session <- connect(mcp)
            held    <- session.open(hold)
            _       <- started.await
            passed  <- session.open(pass)
            first   <- passed.result
            _       <- gate.succeed(())
            second  <- held.result
          yield assertTrue(said(first, "bea"), said(second, "ada"), held.id != passed.id)
        }
      ,
      test("a tool notification is on the call before its result"):
        ZIO.scoped {
          for
            mcp <- ZIO.fromEither(
              Mcp
                .from(Api("session", "0"))
                .flatMap(_.tool[Arg]("tick")(_ => Session.publish(note) *> ZIO.succeed("done")))
            )
            session <- connect(mcp)
            call    <- session.open(tick)
            body    <- call.result
            notes   <- call.notifications
          yield assertTrue(said(body, "done"), notes == Chunk(note))
        }
      ,
      test("cancelling the waiting call leaves the other one running"):
        ZIO.scoped {
          for
            holdStarted <- Promise.make[Nothing, Unit]
            holdStopped <- Promise.make[Nothing, Unit]
            holdGate    <- Promise.make[Nothing, Unit]
            passStarted <- Promise.make[Nothing, Unit]
            passStopped <- Promise.make[Nothing, Unit]
            passGate    <- Promise.make[Nothing, Unit]
            mcp         <- holdAndPass(
              _ => (holdStarted.succeed(()) *> holdGate.await).onInterrupt(holdStopped.succeed(())).as("ada"),
              _ => (passStarted.succeed(()) *> passGate.await).onInterrupt(passStopped.succeed(())).as("bea"),
            )
            session <- connect(mcp)
            held    <- session.open(hold)
            _       <- holdStarted.await
            other   <- session.open(pass)
            _       <- passStarted.await
            _       <- held.cancel
            _       <- holdStopped.await
            still   <- passStopped.poll
            _       <- passGate.succeed(())
            body    <- other.result
            failed  <- held.result.flip
          yield assertTrue(still.isEmpty, said(body, "bea"), failed == McpError.Cancelled(held.id))
        }
      ,
    )
end SessionTransportSpec
