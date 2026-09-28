package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import heddle.mcp.protocol.ToolName
import zio.*
import zio.json.JsonCodec
import zio.json.ast.Json

/** How the one person who can answer replied to a call a view asked to make. */
enum ConsentOutcome derives JsonCodec:
  case AllowOnce, AllowForSession, Rejected, Cancelled, Unavailable

  def allows: Boolean = this == AllowOnce || this == AllowForSession

/** One call a view asked to make, as the user is shown it. */
final case class ConsentRequest(server: ServerName, view: UiUri, tool: ToolName, arguments: Json.Obj)

/** The one gate between a view and a tool call. The iframe is never the answerer, and installing a server is not
  * consent for what its view asks.
  */
trait ConsentGate:
  def decide(request: ConsentRequest): UIO[ConsentOutcome]

object ConsentGate:
  /** No one can answer, so every call a view asks for is refused. */
  val unavailable: ULayer[ConsentGate] =
    ZLayer.succeed(new ConsentGate:
      def decide(request: ConsentRequest): UIO[ConsentOutcome] = ZIO.succeed(ConsentOutcome.Unavailable))

  /** Asks through `ask` (a host's dialog), and remembers `AllowForSession` for the same server, view, and tool as long
    * as the gate lives.
    */
  def remembering(ask: ConsentRequest => UIO[ConsentOutcome]): ULayer[ConsentGate] =
    ZLayer(Ref.make(Set.empty[(ServerName, UiUri, ToolName)]).map { allowed =>
      new ConsentGate:
        def decide(request: ConsentRequest): UIO[ConsentOutcome] =
          val key = (request.server, request.view, request.tool)
          ZIO.ifZIO(allowed.get.map(_.contains(key)))(
            ZIO.succeed(ConsentOutcome.AllowForSession),
            ask(request).tap(o => allowed.update(_ + key).when(o == ConsentOutcome.AllowForSession)),
          )
    })
end ConsentGate
