package heddle.mcp

import heddle.endpoint.OpArgsError
import heddle.error.HeddleError
import heddle.mcp.protocol.{ToolName, ToolNameError}

/** Why `Mcp.from` or `withResources` refused. Found before the server starts, so a bad tool never reaches a client. */
enum McpBuildError(val message: String) extends HeddleError:
  case InvalidToolName(operation: String, reason: ToolNameError)
      extends McpBuildError(s"$operation: ${reason.message}. Rename it with .mcp(\"a_valid_name\").")
  case NotPromotable(tool: String, reason: OpArgsError)
      extends McpBuildError(s"$tool cannot be an MCP tool: ${reason.message}")
  case DuplicateTool(name: ToolName)  extends McpBuildError(s"two tools are named ${name.value}")
  case DuplicateResource(uri: String) extends McpBuildError(s"two resources have the uri $uri")
  case NoSuchTool(name: ToolName)     extends McpBuildError(s"no operation is named ${name.value}")
