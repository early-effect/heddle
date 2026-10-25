package heddle.mcp

import heddle.*
import heddle.mcp.client.{McpCallFailure, ToolResults}
import heddle.mcp.protocol.CallToolResult
import heddle.mcp.server.ToolCall
import zio.*
import zio.test.*

object ResponseHeaderSpec extends ZIOSpecDefault:
  def spec = suite("response headers stay off the tool")(
    test("an outHeader endpoint is not a tool"):
      val ep = Endpoint.get("t").out[String].outHeader[EntityTag].name("tagged").mcp
      assertTrue(
        ToolCall.bound(ep.implement(_ => ZIO.succeed(("hi", EntityTag("a"))))) ==
          Left(McpBuildError.NotPromotable("tagged", OpArgsError.ResponseHeader("ETag")))
      )
    ,
    test("ToolResults does not invent the missing header"):
      val ep = Endpoint.get("t").out[String].outHeader[EntityTag]
      ToolResults.decode(ep, CallToolResult(Chunk.empty)).either.map { got =>
        assertTrue(got == Left(McpCallFailure.NoArguments(OpArgsError.ResponseHeader("ETag"))))
      },
  )
