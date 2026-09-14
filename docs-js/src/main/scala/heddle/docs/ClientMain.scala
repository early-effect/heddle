package heddle.docs

import heddle.docs.widget.*
import specular.client.{Mounter, SpecularClient}
import zio.*

/** Browser entry: remount every `exampleDom` widget on the current page. */
object ClientMain extends ZIOAppDefault:

  val extraMounters: Map[String, Mounter] = Map(
    InteractiveRegistry.ThreeHosts       -> ThreeHostsExplorer.mounter,
    InteractiveRegistry.PathPlayground   -> PathPlayground.mounter,
    InteractiveRegistry.MiddlewareStack  -> MiddlewareStack.mounter,
    InteractiveRegistry.OpenApiPreview   -> OpenApiPreview.mounter,
    InteractiveRegistry.McpCatalog       -> McpCatalog.mounter,
    InteractiveRegistry.JsonRpcInspector -> JsonRpcInspector.mounter,
    InteractiveRegistry.SseEvents        -> SseEvents.mounter,
    InteractiveRegistry.DatastarPatches  -> DatastarPatches.mounter,
  )

  def run = ZIO.scoped {
    SpecularClient.mountAll(extraMounters) *> ZIO.never
  }
end ClientMain
