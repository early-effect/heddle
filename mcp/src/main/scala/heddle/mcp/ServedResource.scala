package heddle.mcp

import heddle.mcp.protocol.{Resource, ResourceContents}
import zio.{Chunk, ZIO}

/** Why a listed resource could not be read just now. Answered as a JSON-RPC internal error. */
final case class ResourceUnavailable(reason: String)

/** A resource a server lists and reads: its `resources/list` entry and how to produce its contents. */
final case class ServedResource[-R](resource: Resource, read: ZIO[R, ResourceUnavailable, Chunk[ResourceContents]])

object ServedResource:
  /** Contents fixed when the server is built. */
  def text(resource: Resource, body: String): ServedResource[Any] =
    ServedResource(resource, ZIO.succeed(Chunk(ResourceContents.Text(resource.uri, resource.mimeType, body, None))))
