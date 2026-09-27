package heddle.mcp

import heddle.ChildCommand
import heddle.mcp.client.{McpClient, McpError, McpStdio}
import heddle.mcp.protocol.{Implementation, ToolName}
import zio.*
import zio.json.ast.Json
import zio.test.*

object McpStdioNodeSpec extends ZIOSpecDefault:
  /** A child that answers every request with its own arguments as `structuredContent`. */
  private val echo =
    """require('readline').createInterface({input: process.stdin}).on('line', l => {
      |  const m = JSON.parse(l)
      |  if (m.id === undefined) return
      |  const args = (m.params && m.params.arguments) || {}
      |  process.stdout.write(JSON.stringify({jsonrpc: '2.0', id: m.id, result: {content: [], structuredContent: args}}) + '\n')
      |})""".stripMargin

  private val settings = McpClient.Settings(Implementation("node-spec", "0.0.1"))

  def spec = suite("McpStdio on Node")(
    test("spawns a child, speaks JSON-RPC over its stdio, and stops it with the scope"):
      ZIO.scoped {
        McpStdio.spawn(ChildCommand("node", Chunk("-e", echo)), settings).flatMap { s =>
          s.callTool(ToolName("echo"), Json.Obj("n" -> Json.Num(7))).map { r =>
            assertTrue(r.structuredContent.contains(Json.Obj("n" -> Json.Num(7))))
          }
        }
      }
    ,
    test("a program that does not exist is a Spawn error"):
      ZIO.scoped(McpStdio.spawn(ChildCommand("/no/such/mcp-server"), settings).flip).map { e =>
        assertTrue(e match
          case McpError.Spawn(_, _) => true
          case _                    => false)
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
end McpStdioNodeSpec
