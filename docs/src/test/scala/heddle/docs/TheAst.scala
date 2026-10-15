package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object TheAst extends DocSpecSuite:

  def doc = page("The AST")(
    md"""
The compiler is small on purpose.

| Value | Role |
| --- | --- |
| `Schema` / `SchemaDoc` | Domain shape every host can read |
| `Endpoint` / `EndpointDoc` | One operation, still unbound |
| `BoundOp` | Endpoint + `In => ZIO[R, E, Out]` |
| `Api` | Named collection of bound ops |
| `OpArgs` | Flatten path, query, JSON body into one argument object |

Interpreters fold that AST:

| Interpreter | Output |
| --- | --- |
| `api.routes` | HTTP |
| `api.openApi` | OpenAPI 3.1 + Swagger UI |
| `Mcp.from(api)` | MCP tools (promoted) + optional catalog |
| CLI (later) | argv / subcommands via `OpArgs` |
""",
    illustrationIO(Hub.Lives.opAnatomy).live.withMountKey(InteractiveRegistry.AstAnatomy),
    section("OpArgs is why MCP (and CLI) can share HTTP")(
      md"""
`OpArgs.promotable` is the tool rule: JSON in, JSON out, and no response header. An endpoint may
use any `BodyCodec`. Path params and query strings flatten into the same JSON object a tool call
(or a future CLI) already knows how to pass. A request header does not: it stays on the HTTP
request. A response header is part of that HTTP output, so an endpoint that declares one stays off
the tool list even when its body is JSON. HTML, text, form, bytes, and event streams stay on HTTP
too. Do not smash them into tools.
""",
      exampleValue {
        OpArgs.promotable(BoxOffice.getShow.doc)
      }.assert(ok => assertTrue(ok)),
      exampleValue {
        OpArgs.inputSchema(BoxOffice.getShow.doc).map(_.jsonSchema.toString).exists(_.contains("id"))
      }.assert(ok => assertTrue(ok)),
      exampleValue {
        val tagged = Endpoint.get("item").out[String].outHeader[EntityTag]
        (OpArgs.promotable(tagged.doc), tagged.doc.successHeader)
      }.assert { case (ok, header) =>
        assertTrue(!ok, header.contains("ETag"))
      },
    ),
    section("Click through EndpointDoc")(
      md"""
Each field names its readers. Click one. That is the AST, not a metaphor.
"""
    ),
  )
end TheAst
