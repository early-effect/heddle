package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import heddle.mcp.protocol.ToolName
import zio.*
import zio.json.*
import zio.json.ast.Json

/** How the one person who can answer replied to a call a view asked to make. */
enum ConsentOutcome derives JsonCodec:
  case AllowOnce, AllowForSession, Rejected, Cancelled, Unavailable

  def allows: Boolean = this == AllowOnce || this == AllowForSession

/** One call a view asked to make, as the user is shown it. `summary` is the tool's own title or description. A host that
  * did not read one leaves it off the wire, and a missing field is the same as none.
  */
final case class ConsentRequest(
    server: ServerName,
    view: UiUri,
    tool: ToolName,
    arguments: Json.Obj,
    summary: Option[String] = None,
) derives JsonCodec

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

  /** Asks through `ask` (a host's one dialog), and remembers `AllowForSession` in a memory of its own for as long as
    * the gate lives.
    */
  def remembering(ask: ConsentRequest => UIO[ConsentOutcome]): ULayer[ConsentGate] =
    ZLayer(ConsentMemory.make.map(_.gate(ask)))
end ConsentGate

/** What the user allowed for the session: `AllowForSession` for a server, view, and tool, however many gates ask. A
  * host with a dialog per frame gives each frame its own gate, so the frame's own dialog answers it, and shares one
  * memory, so "for this session" means the same everywhere on the page.
  */
final class ConsentMemory private (allowed: Ref[Set[(ServerName, UiUri, ToolName)]]):
  /** A gate that answers from this memory, and asks through `ask` for anything it does not hold. */
  def gate(ask: ConsentRequest => UIO[ConsentOutcome]): ConsentGate = new ConsentGate:
    def decide(request: ConsentRequest): UIO[ConsentOutcome] =
      val key = (request.server, request.view, request.tool)
      ZIO.ifZIO(allowed.get.map(_.contains(key)))(
        ZIO.succeed(ConsentOutcome.AllowForSession),
        ask(request).tap(o => allowed.update(_ + key).when(o == ConsentOutcome.AllowForSession)),
      )

object ConsentMemory:
  def make: UIO[ConsentMemory] = Ref.make(Set.empty[(ServerName, UiUri, ToolName)]).map(ConsentMemory(_))

  val layer: ULayer[ConsentMemory] = ZLayer(make)
