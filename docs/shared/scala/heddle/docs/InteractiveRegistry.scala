package heddle.docs

/** Mount keys for live illustrations. Shared by JVM pages, the JS client, and the contract spec. */
object InteractiveRegistry:
  val LandingPoster: String    = "landing-poster"
  val HostFanout: String       = "host-fanout"
  val OpAnatomy: String        = "op-anatomy"
  val EffectTrace: String      = "effect-trace"
  val SwaggerWalk: String      = "swagger-walk"
  val HarnessWalk: String      = "harness-walk"
  val DeskApp: String          = "desk-app"
  val PathPlayground: String   = "path-playground"
  val MiddlewareStack: String  = "middleware-stack"
  val OpenApiPreview: String   = "openapi-preview"
  val PromoteVsCatalog: String = "promote-vs-catalog"
  val JsonRpcInspector: String = "jsonrpc-inspector"
  val SseTape: String          = "sse-tape"
  val DatastarPatches: String  = "datastar-patches"

  val liveKeys: Set[String] = Set(
    HostFanout,
    OpAnatomy,
    EffectTrace,
    SwaggerWalk,
    HarnessWalk,
    DeskApp,
    PathPlayground,
    MiddlewareStack,
    OpenApiPreview,
    PromoteVsCatalog,
    JsonRpcInspector,
    SseTape,
    DatastarPatches,
  )
end InteractiveRegistry
