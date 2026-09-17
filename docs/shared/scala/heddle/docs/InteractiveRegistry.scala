package heddle.docs

/** Mount keys for live illustrations. Shared by JVM pages, the JS client, and the contract spec. */
object InteractiveRegistry:
  val LandingPoster: String    = "landing-poster"
  val HubPoster: String        = "hub-poster"
  val HostsPoster: String      = "hosts-poster"
  val HostFanout: String       = "host-fanout"
  val OpAnatomy: String        = "op-anatomy"
  val AstAnatomy: String       = "ast-anatomy"
  val EffectTrace: String      = "effect-trace"
  val EffectWalk: String       = "effect-walk"
  val SwaggerWalk: String      = "swagger-walk"
  val SchemaPoster: String     = "schema-poster"
  val EndpointsOpenApi: String = "endpoints-openapi"
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
    HubPoster,
    HostsPoster,
    HostFanout,
    OpAnatomy,
    AstAnatomy,
    EffectTrace,
    EffectWalk,
    SwaggerWalk,
    SchemaPoster,
    EndpointsOpenApi,
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
