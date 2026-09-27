package heddle.mcp.client

import heddle.endpoint.{Endpoint, OpArgs}
import heddle.http.Url
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** A connected MCP server. `request` is the one primitive; the rest decode its results into protocol types. */
trait McpSession:
  /** The handshake this session negotiated. */
  def era: Era

  /** What the server said it is, when it said. */
  def server: Option[Implementation]

  /** One request and its raw result object. A JSON-RPC error answer is `McpError.Rpc`. */
  def request(req: ClientRequest): IO[McpError, Json.Obj]

  final def ping: IO[McpError, Unit] = request(ClientRequest.Ping).unit

  /** Every tool, following `nextCursor` to the end. */
  final def listTools: IO[McpError, Chunk[Tool]] =
    McpSession.pages(c => request(ClientRequest.ListTools(c)).flatMap(McpSession.as[ListToolsResult]))(r =>
      (r.tools, r.nextCursor)
    )

  final def callTool(name: ToolName, arguments: Json.Obj): IO[McpError, CallToolResult] =
    request(ClientRequest.CallTool(name, arguments)).flatMap(McpSession.as[CallToolResult])

  /** Every resource, following `nextCursor` to the end. */
  final def listResources: IO[McpError, Chunk[Resource]] =
    McpSession.pages(c => request(ClientRequest.ListResources(c)).flatMap(McpSession.as[ListResourcesResult]))(r =>
      (r.resources, r.nextCursor)
    )

  final def readResource(uri: String): IO[McpError, Chunk[ResourceContents]] =
    request(ClientRequest.ReadResource(uri)).flatMap(McpSession.as[ReadResourceResult]).map(_.contents)

  /** Pins an endpoint the server exposes as a tool; `apply` sends its typed input and reads its typed output. */
  final def call[In, Err, Out](ep: Endpoint[In, Err, Out]): McpSession.CallPartiallyApplied[In, Err, Out] =
    McpSession.CallPartiallyApplied(this, ep)
end McpSession

object McpSession:
  final class CallPartiallyApplied[In, Err, Out](session: McpSession, ep: Endpoint[In, Err, Out]):
    def apply(in: In): IO[McpCallFailure[Err], Out] =
      val doc = ep.doc
      for
        name <- ZIO.fromEither(ToolName.from(doc.toolName)).mapError(McpCallFailure.BadToolName(_))
        args <- ZIO
          .fromEither(OpArgs.arguments(doc, ep.toRequest(in, Url.root)))
          .mapError(McpCallFailure.NoArguments(_))
        result <- session.callTool(name, args).mapError(McpCallFailure.Session(_))
        out    <- ToolResults.decode(ep, result)
      yield out
    end apply
  end CallPartiallyApplied

  private[client] def as[A: JsonDecoder](result: Json.Obj): IO[McpError, A] =
    ZIO.fromEither(result.as[A]).mapError(McpError.Protocol(_))

  /** Follows cursors until the server stops. A cursor the server already sent is a protocol error, not a loop. */
  private[client] def pages[P, A](fetch: Option[String] => IO[McpError, P])(
      split: P => (Chunk[A], Option[String])
  ): IO[McpError, Chunk[A]] =
    def go(cursor: Option[String], seen: Set[String], acc: Chunk[A]): IO[McpError, Chunk[A]] =
      fetch(cursor).map(split).flatMap {
        case (items, None)                          => ZIO.succeed(acc ++ items)
        case (_, Some(next)) if seen.contains(next) => ZIO.fail(McpError.Protocol(s"cursor $next repeated"))
        case (items, Some(next))                    => go(Some(next), seen + next, acc ++ items)
      }
    go(None, Set.empty, Chunk.empty)
  end pages
end McpSession
