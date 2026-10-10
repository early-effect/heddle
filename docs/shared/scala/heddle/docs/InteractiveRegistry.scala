package heddle.docs

import specular.MountKey

/** Mount keys for live illustrations. Shared by JVM pages, the JS client, and the contract spec.
  *
  * Literals, not strings: `withMountKey`'s `String` overload only accepts a literal.
  */
object InteractiveRegistry:
  val HubPoster: MountKey        = MountKey("hub-poster")
  val FrontHosts: MountKey       = MountKey("front-hosts")
  val HostsPoster: MountKey      = MountKey("hosts-poster")
  val HostFanout: MountKey       = MountKey("host-fanout")
  val OpAnatomy: MountKey        = MountKey("op-anatomy")
  val AstAnatomy: MountKey       = MountKey("ast-anatomy")
  val EffectTrace: MountKey      = MountKey("effect-trace")
  val EffectWalk: MountKey       = MountKey("effect-walk")
  val SwaggerWalk: MountKey      = MountKey("swagger-walk")
  val SchemaPoster: MountKey     = MountKey("schema-poster")
  val EndpointsOpenApi: MountKey = MountKey("endpoints-openapi")
  val HarnessWalk: MountKey      = MountKey("harness-walk")
  val DeskApp: MountKey          = MountKey("desk-app")
  val PathPlayground: MountKey   = MountKey("path-playground")
  val MiddlewareStack: MountKey  = MountKey("middleware-stack")
  val OpenApiPreview: MountKey   = MountKey("openapi-preview")
  val PromoteVsCatalog: MountKey = MountKey("promote-vs-catalog")
  val JsonRpcInspector: MountKey = MountKey("jsonrpc-inspector")
  val SseTape: MountKey          = MountKey("sse-tape")
  val DatastarPatches: MountKey  = MountKey("datastar-patches")

  val liveKeys: Set[MountKey] = Set(
    HubPoster,
    FrontHosts,
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
