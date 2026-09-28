package heddle.mcp.apps.host

import heddle.error.HeddleError
import heddle.mcp.apps.{MetaProblem, ScriptHash, Sha256, UiMeta, UiPolicy, UiUri}
import heddle.mcp.client.{McpError, McpSession}
import heddle.mcp.protocol.{CallToolResult, ResourceContents, Tool, ToolName}
import java.nio.charset.StandardCharsets
import zio.Chunk
import zio.json.ast.Json

/** The SHA-256 of a view's bytes, in hex: what a host pins. */
opaque type Digest = String

object Digest:
  def of(text: String): Digest = Sha256.hex(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  extension (d: Digest) def hex: String = d

/** One MCP server the host is connected to, under the host's name for it. */
final case class AppServer(name: ServerName, session: McpSession)

/** The model's call of a tool that opens a view: what the view renders. */
final case class Launched(tool: ToolName, input: Json.Obj, result: CallToolResult)

/** Why a host did not mount a view. Every case fails closed: nothing is framed. */
enum MountRefusal(val message: String) extends HeddleError:
  case NotListed(tool: ToolName) extends MountRefusal(s"${tool.value} is not in the server's tools/list")
  case NoView(tool: ToolName)    extends MountRefusal(s"${tool.value} does not name a ui:// view")
  case NotAnApp(uri: UiUri, mimeType: Option[String])
      extends MountRefusal(s"${uri.value} is ${mimeType.getOrElse("untyped")}, not ${UiMeta.MimeType}")
  case Unreadable(uri: UiUri, reason: String) extends MountRefusal(s"${uri.value}: $reason")
  case HashMismatch(uri: UiUri, pinned: Digest, served: Digest)
      extends MountRefusal(s"${uri.value} changed since it was pinned (${pinned.hex} is now ${served.hex})")
  case Session(error: McpError) extends MountRefusal(error.message)

/** A view as the server links it: its `ui://` resource, the tool that opened it, and the tools it may call. The host
  * reads all of this from the live `tools/list`, never from the view.
  */
final case class LinkedView(server: ServerName, uri: UiUri, launch: Tool, appTools: Set[ToolName]):
  /** The shed gate: the tool is app-visible, linked to this view, and in this server's live list. */
  def admits(tool: ToolName): Boolean = appTools.contains(tool)

object LinkedView:
  def of(server: ServerName, launch: ToolName, live: Chunk[Tool]): Either[MountRefusal, LinkedView] =
    for
      tool <- live.find(_.name == launch).toRight(MountRefusal.NotListed(launch))
      uri  <- UiMeta.decodeTool(tool.meta)._1.resourceUri.toRight(MountRefusal.NoView(launch))
    yield
      val linked = live.filter { t =>
        val ui = UiMeta.decodeTool(t.meta)._1
        ui.resourceUri.contains(uri) && ui.visibility.app
      }
      LinkedView(server, uri, tool, linked.map(_.name).toSet)
end LinkedView

/** A view's document as the server sent it, with the policy it asks for and the script hashes it declared. What the
  * host could not read is in `problems`; it is dropped, never widened.
  */
final case class ViewResource(html: String, ask: UiPolicy, scripts: Chunk[ScriptHash], problems: Chunk[MetaProblem]):
  def digest: Digest = Digest.of(html)

object ViewResource:
  /** Exactly the linked `ui://`, typed `text/html;profile=mcp-app`, as text or base64 UTF-8. */
  def of(uri: UiUri, contents: Chunk[ResourceContents]): Either[MountRefusal, ViewResource] =
    for
      content <- contents.find(_.uri == uri.value).toRight(MountRefusal.Unreadable(uri, "no contents for this uri"))
      _       <- Either.cond(
        content.mimeType.contains(UiMeta.MimeType),
        (),
        MountRefusal.NotAnApp(uri, content.mimeType),
      )
      html <- content match
        case ResourceContents.Text(_, _, text, _) => Right(text)
        case ResourceContents.Blob(_, _, blob, _) =>
          heddle.crypto.Base64
            .decode(blob)
            .map(bytes => String(bytes.toArray, StandardCharsets.UTF_8))
            .left
            .map(e => MountRefusal.Unreadable(uri, s"the blob is not base64: ${e.message}"))
    yield
      val (ask, policyProblems) = UiMeta.decodeResource(content.meta)
      val (scripts, badScripts) = UiMeta.decodeScripts(content.meta)
      ViewResource(html, ask, scripts, policyProblems ++ badScripts)
end ViewResource
