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
`OpArgs.promotable` requires JSON in and JSON out. Path params and query strings flatten into
the same JSON object a tool call (or a future CLI) already knows how to pass. Form, bytes, and
SSE stay on HTTP. Do not smash them into tools.
""",
      exampleValue {
        OpArgs.promotable(BoxOffice.getShow.doc)
      }.assert(ok => assertTrue(ok)),
      exampleValue {
        OpArgs.inputSchema(BoxOffice.getShow.doc).map(_.jsonSchema.toString).exists(_.contains("id"))
      }.assert(ok => assertTrue(ok)),
    ),
    section("Click through EndpointDoc")(
      md"""
Each field names its readers. Click one. That is the AST, not a metaphor.
"""
    ),
  )
end TheAst
