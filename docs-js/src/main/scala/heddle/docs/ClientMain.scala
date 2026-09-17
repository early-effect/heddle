package heddle.docs

import heddle.docs.ui.Hub
import specular.client.{Mounter, SpecularClient}
import zio.*

/** Browser entry: remount every live illustration on the current page. */
object ClientMain extends ZIOAppDefault:

  val extraMounters: Map[String, Mounter] = Map(
    InteractiveRegistry.LandingPoster    -> Mounter.fromAscent(Hub.Lives.landingPoster),
    InteractiveRegistry.HostFanout       -> Mounter.fromAscent(Hub.Lives.hostFanout),
    InteractiveRegistry.OpAnatomy        -> Mounter.fromAscent(Hub.Lives.opAnatomy),
    InteractiveRegistry.EffectTrace      -> Mounter.fromAscent(Hub.Lives.effectTrace),
    InteractiveRegistry.SwaggerWalk      -> Mounter.fromAscent(Hub.Lives.swaggerWalk),
    InteractiveRegistry.HarnessWalk      -> Mounter.fromAscent(Hub.Lives.harnessWalk),
    InteractiveRegistry.DeskApp          -> Mounter.fromAscent(Hub.Lives.desk),
    InteractiveRegistry.PathPlayground   -> Mounter.fromAscent(Hub.Lives.pathPlay),
    InteractiveRegistry.MiddlewareStack  -> Mounter.fromAscent(Hub.Lives.middleware),
    InteractiveRegistry.OpenApiPreview   -> Mounter.fromAscent(Hub.Lives.openApi),
    InteractiveRegistry.PromoteVsCatalog -> Mounter.fromAscent(Hub.Lives.catalog),
    InteractiveRegistry.JsonRpcInspector -> Mounter.fromAscent(Hub.Lives.rpc),
    InteractiveRegistry.SseTape          -> Mounter.fromAscent(Hub.Lives.sseTape),
    InteractiveRegistry.DatastarPatches  -> Mounter.fromAscent(Hub.Lives.datastar),
  )

  def run = ZIO.scoped {
    SpecularClient.mountAll(extraMounters) *> ZIO.never
  }
end ClientMain
