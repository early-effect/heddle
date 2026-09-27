package heddle.mcp.client

import heddle.client.ClientError
import heddle.error.HeddleError
import heddle.http.Status
import heddle.mcp.protocol.RpcError

/** Why an MCP request produced no result. A tool's own failure is not one of these: it is an `isError` result. */
enum McpError(val message: String) extends HeddleError:
  /** The HTTP exchange failed before an answer arrived. */
  case Transport(error: ClientError) extends McpError(error.message)

  /** The pipe to a stdio server failed. */
  case Pipe(cause: Throwable) extends McpError(s"MCP pipe failed: $cause")

  /** The server process could not be started. */
  case Spawn(command: String, cause: Throwable) extends McpError(s"Cannot start MCP server `$command`: $cause")

  /** The server answered with a JSON-RPC error. */
  case Rpc(error: RpcError) extends McpError(s"MCP error ${error.code}: ${error.text}")

  /** HTTP answered with a status and no JSON-RPC body. */
  case Http(status: Status) extends McpError(s"MCP endpoint answered ${status.code} with no JSON-RPC body")

  /** The server's answer is not valid MCP. */
  case Protocol(reason: String) extends McpError(s"Not an MCP answer: $reason")

  /** The server ended the session: its pipe closed, or its process exited. */
  case Closed extends McpError("MCP session closed")

  /** No answer arrived within the session's request timeout. */
  case TimedOut(method: String) extends McpError(s"No answer to $method in time")
end McpError
