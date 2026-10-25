package heddle.mcp

import heddle.*
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

final case class ToolArg(name: String) derives Schema, JsonCodec

object StdioSessionSpec extends ZIOSpecDefault:
  private val note: Message.Notification =
    Message.Notification("notifications/progress", Json.Obj("token" -> Json.Str("tick")))

  private val initialize: Message.Request =
    Message.request(
      RequestId.Num(1),
      ClientRequest.Initialize(ProtocolVersion.Legacy, Json.Obj(), None),
    )

  private enum Boom:
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Closed
    case Undecodable(line: String)
    case Pipe(error: Throwable)

  private def server(
      a: ToolArg => UIO[String],
      b: ToolArg => UIO[String],
  ): ZIO[Any, Boom, Mcp[Any]] =
    ZIO
      .fromEither(
        Mcp
          .from(Api("spec", "0"))
          .flatMap(_.tool[ToolArg]("tool_a")(a))
          .flatMap(_.tool[ToolArg]("tool_b")(b))
      )
      .mapError(Boom.Build(_))

  private def call(tool: "tool_a" | "tool_b", id: RequestId, name: String): Message.Request =
    val named = tool match
      case "tool_a" => ToolName("tool_a")
      case "tool_b" => ToolName("tool_b")
    Message.stateless(id, ClientRequest.CallTool(named, Json.Obj("name" -> Json.Str(name))))

  private def sessionCall(id: RequestId, name: String): Message.Request =
    Message.request(id, ClientRequest.CallTool(ToolName("tool_a"), Json.Obj("name" -> Json.Str(name))))

  private def write(pipe: LinePipe, msg: Message): ZIO[Any, Boom, Unit] =
    pipe.writeLine(msg.json.toJson).mapError(Boom.Pipe(_))

  private def writeRaw(pipe: LinePipe, line: String): ZIO[Any, Boom, Unit] =
    pipe.writeLine(line).mapError(Boom.Pipe(_))

  private def read(pipe: LinePipe): ZIO[Any, Boom, Message] =
    pipe.readLine.mapError(Boom.Pipe(_)).flatMap {
      case None => ZIO.fail(Boom.Closed)
      case Some(line) =>
        line.fromJson[Json] match
          case Left(_) => ZIO.fail(Boom.Undecodable(line))
          case Right(json) =>
            Message.decode(json) match
              case Left(_)    => ZIO.fail(Boom.Undecodable(line))
              case Right(msg) => ZIO.succeed(msg)
    }

  private def open[A](mcp: Mcp[Any])(use: LinePipe => ZIO[Any, Boom, A]): ZIO[Any, Boom, A] =
    LinePipe.connected.flatMap { (client, remote) =>
      mcp.stdio(remote).fork.flatMap(server => use(client).ensuring(server.interrupt.unit))
    }

  private def resultIs(msg: Message, id: RequestId, needle: String): TestResult =
    msg match
      case Message.Result(got, body) => assertTrue(got == id, body.toJson.contains(needle))
      case other                     => assertTrue(false, other.json.toJson == s"$id $needle")

  def spec = suite("stdio stays open while a tool runs")(
    test("a later tool result is the next line while the earlier tool is still waiting"):
      for
        started <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          in => started.succeed(()) *> gate.await *> ZIO.succeed(s"a:${in.name}"),
          in => ZIO.succeed(s"b:${in.name}"),
        )
        out <- open(mcp) { client =>
          for
            _      <- write(client, call("tool_a", RequestId.Num(1), "ada"))
            _      <- started.await
            _      <- write(client, call("tool_b", RequestId.Num(2), "bea"))
            first  <- read(client)
            _      <- gate.succeed(())
            second <- read(client)
          yield resultIs(first, RequestId.Num(2), "b:bea") && resultIs(second, RequestId.Num(1), "a:ada")
        }
      yield out
    ,
    test("a tool notification is one whole line ahead of its result"):
      for
        mcp <- server(_ => Session.publish(note) *> ZIO.succeed("done"), _ => ZIO.succeed("other"))
        out <- open(mcp) { client =>
          for
            _      <- write(client, call("tool_a", RequestId.Num(3), "ada"))
            first  <- read(client)
            second <- read(client)
          yield assertTrue(first == note) && resultIs(second, RequestId.Num(3), "done")
        }
      yield out
    ,
    test("cancelling a waiting tool writes no result, then the next tool answers"):
      for
        started <- Promise.make[Nothing, Unit]
        stopped <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          _ => (started.succeed(()) *> gate.await).onInterrupt(stopped.succeed(())).as("a"),
          _ => ZIO.succeed("b-finished"),
        )
        out <- open(mcp) { client =>
          for
            _    <- write(client, call("tool_a", RequestId.Num(4), "ada"))
            _    <- started.await
            _    <- write(client, Cancelled(RequestId.Num(4), Some("stop")).message)
            _    <- stopped.await
            _    <- write(client, call("tool_b", RequestId.Num(5), "bea"))
            line <- read(client)
          yield resultIs(line, RequestId.Num(5), "b-finished")
        }
      yield out
    ,
    test("a parse error is the next line while a tool is still running"):
      for
        started <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          in => started.succeed(()) *> gate.await *> ZIO.succeed(s"a:${in.name}"),
          _ => ZIO.succeed("other"),
        )
        out <- open(mcp) { client =>
          for
            _      <- write(client, call("tool_a", RequestId.Num(6), "ada"))
            _      <- started.await
            _      <- writeRaw(client, "{")
            bad    <- read(client)
            _      <- gate.succeed(())
            second <- read(client)
          yield isParseError(bad) && resultIs(second, RequestId.Num(6), "a:ada")
        }
      yield out
    ,
    test("initialize already in the pipe is answered before the call runs"):
      for
        started <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          in => started.succeed(()) *> gate.await *> ZIO.succeed(in.name),
          _ => ZIO.succeed("other"),
        )
        out <- open(mcp) { client =>
          for
            _      <- write(client, initialize)
            _      <- write(client, sessionCall(RequestId.Num(2), "later"))
            first  <- read(client)
            _      <- started.await
            _      <- gate.succeed(())
            second <- read(client)
          yield isInitialize(first) && resultIs(second, RequestId.Num(2), "later")
        }
      yield out
    ,
    test("stdin EOF joins the waiting tool and its result is still written"):
      for
        started <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          in => started.succeed(()) *> gate.await *> ZIO.succeed(s"a:${in.name}"),
          _ => ZIO.succeed("other"),
        )
        (client, remote) <- LinePipe.connected
        running          <- mcp.stdio(remote).fork
        out              <-
          (for
            _    <- write(client, call("tool_a", RequestId.Num(8), "ada"))
            _    <- started.await
            _    <- client.close
            _    <- gate.succeed(())
            exit <- running.await
            line <- read(client)
          yield assertTrue(exit.isSuccess) && resultIs(line, RequestId.Num(8), "a:ada"))
            .ensuring(running.interrupt.unit)
      yield out
    ,
    test("interrupting stdio interrupts the in-flight tool"):
      for
        started <- Promise.make[Nothing, Unit]
        stopped <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        mcp <- server(
          _ => (started.succeed(()) *> gate.await).onInterrupt(stopped.succeed(())).as("a"),
          _ => ZIO.succeed("b"),
        )
        (client, remote) <- LinePipe.connected
        server           <- mcp.stdio(remote).fork
        _                <- write(client, call("tool_a", RequestId.Num(7), "ada"))
        _                <- started.await
        _                <- server.interrupt
        _                <- stopped.await
      yield assertCompletes
    ,
  )

  private def isParseError(msg: Message): TestResult =
    msg match
      case Message.Error(None, _: RpcError.ParseError) => assertTrue(true)
      case other                                       => assertTrue(false, other.json.toJson.contains("Parse error"))

  private def isInitialize(msg: Message): TestResult =
    msg match
      case Message.Result(RequestId.Num(1), body) => assertTrue(body.toJson.contains("2025-11-25"))
      case other                                  => assertTrue(false, other.json.toJson.contains("2025-11-25"))
end StdioSessionSpec
