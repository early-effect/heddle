package heddle.mcp

import heddle.error.HeddleError
import heddle.mcp.protocol.ToolName

/** Why `Mcp.from` refused an API. Found before the server starts, so a bad tool never reaches a client. */
enum McpBuildError(val message: String) extends HeddleError:
  case InvalidToolName(operation: String, reason: String)
      extends McpBuildError(s"$operation: $reason. Rename it with .mcp(\"a_valid_name\").")
  case NotPromotable(tool: String, reason: String) extends McpBuildError(s"$tool cannot be an MCP tool: $reason")
  case DuplicateTool(name: ToolName)               extends McpBuildError(s"two tools are named ${name.value}")
