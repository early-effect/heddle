package heddle.apps.browser.host

import heddle.mcp.apps.Origin
import heddle.mcp.apps.host.{Ending, Generation, ServerName}
import heddle.mcp.protocol.ToolName
import zio.Chunk
import zio.json.*

/** One server the page connects to, and the tool the page calls as the model would. */
final case class Launch(server: ServerName, url: String, tool: ToolName) derives JsonCodec

/** The page's config: its own origin, the relay's (none for an opaque `srcdoc` relay), and the servers. */
final case class PageConfig(host: Origin, relay: Option[Origin], launches: Chunk[Launch]) derives JsonCodec

object PageConfig:
  val ElementId = "heddle-host-config"

/** What happened to one press of a Launch button. */
enum MountOutcome derives JsonCodec:
  case Mounted(server: ServerName, generation: Generation)
  case Refused(server: ServerName, refusal: String)
  case Ended(server: ServerName, ending: Ending)
