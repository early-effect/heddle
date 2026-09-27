package heddle.mcp.apps.ui

import heddle.endpoint.{Endpoint, OpArgs}
import heddle.mcp.client.{McpCallFailure, ToolResults}
import zio.{UIO, ZIO}
import zio.json.ast.Json

/** The launch tool's lifecycle, as its view renders it: the model writing the input, the call, and how it ended. */
enum Run[+In, +Err, +Out]:
  /** The model is still writing the input. `partial` is the host's best guess at it: shown, never trusted. */
  case Pending(partial: Option[Json.Obj])
  case Called(in: In)
  case Returned(in: In, out: Out)
  case Failed(in: In, failure: McpCallFailure[Err])
  case Cancelled(reason: Option[String])

  /** The host sent an input or a result the launch tool's types cannot read, or a result with no input before it. */
  case Unreadable(detail: String)
end Run

object Run:
  /** Where a view starts: nothing has arrived yet. */
  val waiting: Run[Nothing, Nothing, Nothing] = Pending(None)

  /** The next state after a host notification about the launch tool. Context changes leave the run where it is. */
  def step[In, Err, Out](
      ep: Endpoint[In, Err, Out]
  )(run: Run[In, Err, Out], note: HostNotification): UIO[Run[In, Err, Out]] =
    note match
      case HostNotification.ToolInputPartial(partial) =>
        ZIO.succeed(run match
          case Pending(_) => Pending(Some(partial))
          case settled    => settled)
      case HostNotification.ToolInput(arguments) => input(ep, arguments)
      case HostNotification.ToolResult(result)   =>
        run match
          case Called(in) => ToolResults.decode(ep, result).fold(Failed(in, _), Returned(in, _))
          case _          => ZIO.succeed(Unreadable("a tool result arrived before its input"))
      case HostNotification.ToolCancelled(reason) => ZIO.succeed(Cancelled(reason))
      case HostNotification.HostContextChanged(_) => ZIO.succeed(run)

  /** The arguments read the way the server reads them: as the request the endpoint would have received. */
  private def input[In, Err, Out](ep: Endpoint[In, Err, Out], arguments: Json.Obj): UIO[Run[In, Err, Out]] =
    OpArgs.request(ep.doc, arguments) match
      case Left(e)    => ZIO.succeed(Unreadable(e.message))
      case Right(req) =>
        ep.path.matches(req.path) match
          case None       => ZIO.succeed(Unreadable(s"the input does not fill ${ep.doc.toolName}'s path"))
          case Some(path) =>
            ep.decodeIn(path, req)
              .foldZIO(
                res => res.body.utf8.orElseSucceed(s"status ${res.status.code}").map(detail => Unreadable(detail)),
                in => ZIO.succeed(Called(in)),
              )
end Run
