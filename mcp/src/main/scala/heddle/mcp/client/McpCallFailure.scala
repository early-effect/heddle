package heddle.mcp.client

import heddle.endpoint.{BodyError, OpArgsError}
import heddle.mcp.protocol.ToolNameError

/** Why a typed MCP call to an `Endpoint[In, E, Out]` did not produce an `Out`. */
enum McpCallFailure[+E]:
  /** The tool answered with one of the endpoint's declared errors. */
  case Domain(error: E)

  /** The tool answered `isError` with no error the endpoint declares (bad arguments, a failed input read). */
  case Failed(message: String)

  /** No result: the session or transport failed, or the server answered with a JSON-RPC error. */
  case Session(error: McpError)

  /** A result arrived that the endpoint's codecs reject. */
  case Undecodable(reason: BodyError)

  /** The endpoint declares an output, and the result has no `structuredContent` to read it from. */
  case NoStructuredContent

  /** The endpoint's tool name is outside the MCP grammar. */
  case BadToolName(reason: ToolNameError)

  /** The endpoint's input has no argument form (a streamed or non-JSON body, a repeated query). */
  case NoArguments(reason: OpArgsError)
end McpCallFailure
