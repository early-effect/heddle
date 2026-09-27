package heddle.mcp.client

/** Why a typed MCP call to an `Endpoint[In, E, Out]` did not produce an `Out`. */
enum McpCallFailure[+E]:
  /** The tool answered with one of the endpoint's declared errors. */
  case Domain(error: E)

  /** The tool answered `isError` with no error the endpoint declares (bad arguments, a failed input read). */
  case Failed(message: String)

  /** No result: the session or transport failed, or the server answered with a JSON-RPC error. */
  case Session(error: McpError)

  /** A result arrived that the endpoint's codecs reject. */
  case Undecodable(reason: String)

  /** The endpoint has no MCP form: its tool name is outside the grammar, or its input has no argument form. */
  case NotATool(reason: String)
end McpCallFailure
