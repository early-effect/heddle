package heddle.docs

/** Mount keys for `exampleDom` widgets. Shared by JVM pages, the JS client, and the contract spec. */
object InteractiveRegistry:
  val ThreeHosts: String       = "three-hosts"
  val PathPlayground: String   = "path-playground"
  val MiddlewareStack: String  = "middleware-stack"
  val OpenApiPreview: String   = "openapi-preview"
  val McpCatalog: String       = "mcp-catalog"
  val JsonRpcInspector: String = "jsonrpc-inspector"
  val SseEvents: String        = "sse-events"
  val DatastarPatches: String  = "datastar-patches"

  val domKeys: Set[String] = Set(
    ThreeHosts,
    PathPlayground,
    MiddlewareStack,
    OpenApiPreview,
    McpCatalog,
    JsonRpcInspector,
    SseEvents,
    DatastarPatches,
  )
end InteractiveRegistry
