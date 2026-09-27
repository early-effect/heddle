package heddle.mcp.client

import heddle.{ChildCommand, ChildPipe}
import zio.*

/** An MCP server started as a child process and spoken to over its stdio. JVM and Node; the scope owns the process. */
object McpStdio:
  def spawn(command: ChildCommand, settings: McpClient.Settings): ZIO[Scope, McpError, McpSession] =
    ChildPipe
      .spawn(command)
      .mapError(f => McpError.Spawn(f.command.render, f.cause))
      .flatMap(McpClient.pipe(_, settings))
