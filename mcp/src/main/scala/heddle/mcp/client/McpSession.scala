package heddle.mcp.client

import heddle.endpoint.{Endpoint, OpArgs}
import heddle.http.{Response, Status, Url}
import heddle.mcp.ToolShapes
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
        name   <- ZIO.fromEither(ToolName.from(doc.toolName)).mapError(McpCallFailure.NotATool(_))
        args   <- ZIO.fromEither(OpArgs.arguments(doc, ep.toRequest(in, Url.root))).mapError(McpCallFailure.NotATool(_))
        result <- session.callTool(name, args).mapError(McpCallFailure.Session(_))
        out    <- if result.failed then ZIO.fail(failure(result)) else success(result)
      yield out

    private def failure(result: CallToolResult): McpCallFailure[Err] =
      val typed = result.structuredContent
        .map(ToolShapes.error(ep.doc).unwrap)
        .flatMap(json => ep.errors.decodeJson(json.toJson))
      typed match
        case Some(Right(e)) => McpCallFailure.Domain(e)
        case _              => McpCallFailure.Failed(text(result))

    private def success(result: CallToolResult): IO[McpCallFailure[Err], Out] =
      (ToolShapes.output(ep.doc), ep.outputCodec) match
        case (None, _) =>
          ep.decodeOut(Response.empty(Status.NoContent)).mapError(McpCallFailure.Undecodable(_))
        case (Some(shape), Some(codec)) =>
          result.structuredContent match
            case None     => ZIO.fail(McpCallFailure.Undecodable("the result has no structuredContent"))
            case Some(sc) =>
              ZIO.fromEither(codec.decoder.decodeJson(shape.unwrap(sc).toJson)).mapError(McpCallFailure.Undecodable(_))
        case (Some(_), None) =>
          ZIO.fail(McpCallFailure.Undecodable("the endpoint's output has a schema but no JSON codec"))

    private def text(result: CallToolResult): String =
      result.content.collect { case ContentBlock.Text(t, _) => t }.mkString("\n")
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
