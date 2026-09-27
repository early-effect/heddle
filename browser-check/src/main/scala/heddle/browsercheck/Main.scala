package heddle.browsercheck

import heddle.*
import heddle.mcp.{ToolShapes, apps}
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** What an MCP App view running in a browser links: endpoint description, argument mapping, result shapes, and the
  * wire protocol. `browserCheck` links this as an ES module and fails if the output imports a Node module.
  */
object Main extends ZIOAppDefault:
  final case class Item(id: Int, name: String) derives Schema, JsonCodec

  private val getItem = Endpoint.get("items" / int("id")).out[Item].outError[String](Status.NotFound).mcp

  private val shed = apps.Shed(apps.UiUri("ui://check/view"), "Check", apps.Grant.launch(getItem))(
    (again = apps.Grant.app(getItem))
  )

  def run =
    val args    = OpArgs.arguments(getItem.doc, getItem.toRequest(7, Url.root))
    val request = args.map(a => Message.stateless(RequestId.Num(1), ClientRequest.CallTool(ToolName("get_items_id"), a)))
    val result  = CallToolResult(Chunk(ContentBlock.Text("{}")), Some(Json.Obj("id" -> Json.Num(7))))
    val decoded = ToolShapes.output(getItem.doc).map(shape => result.structuredContent.map(shape.unwrap))
    val policy  = apps.UiMeta.decodeResource(Some(apps.UiMeta.encodeResource(shed.policy)))
    val clamped = apps.Clamp[apps.UiPolicy, apps.HostPolicy].clamp(policy._1, apps.HostPolicy.closed)
    val pinned  = apps.Sha256.hex(Chunk.fromArray("view".getBytes))
    Console.printLine(
      s"${request.map(_.json.toJson)} ${decoded.map(_.toJson)} ${getItem.errors.decodeJson("\"x\"")} " +
        s"${shed.grants.map(_.toolName)} $clamped $pinned"
    )
