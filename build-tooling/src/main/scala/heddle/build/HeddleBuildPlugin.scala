package heddle.build

import histos.plugin.*
import zio.*

trait RelayBrowserTasks:
  def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput]

object RelayBrowserTasks:
  /** B1 replaces this registration with the verified relay/browser implementation. */
  val pending: ULayer[RelayBrowserTasks] = ZLayer.succeed(new RelayBrowserTasks:
    def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.fail(PluginRunError.Unverified(SourceId("heddle.b1"))))

trait DocsTranscriptTask:
  def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput]

object DocsTranscriptTask:
  /** B2 replaces this registration with the verified transcript implementation. */
  val pending: ULayer[DocsTranscriptTask] = ZLayer.succeed(new DocsTranscriptTask:
    def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.fail(PluginRunError.Unverified(SourceId("heddle.b2"))))

final class HeddleBuildPlugin(relayBrowser: RelayBrowserTasks, transcript: DocsTranscriptTask) extends HistosPlugin:
  val id: PluginId             = TaskSpec.pluginId
  val contribute: Contribution =
    Contribution((TaskSpec.relayBrowser ++ TaskSpec.docs).map(_.declared), Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    if request.id.plugin != id then ZIO.fail(PluginRunError.Unknown(request.id))
    else
      TaskSpec.relayBrowser.find(_.name == request.id.name) match
        case Some(task) => relayBrowser.run(task, request)
        case None       =>
          TaskSpec.docs.find(_.name == request.id.name) match
            case Some(task) => transcript.run(task, request)
            case None       => ZIO.fail(PluginRunError.Unknown(request.id))
end HeddleBuildPlugin

object HeddleBuildPlugin:
  val layer: URLayer[RelayBrowserTasks & DocsTranscriptTask, HeddleBuildPlugin] =
    pluginWith[HeddleBuildPlugin, RelayBrowserTasks, DocsTranscriptTask]: (relayBrowser, transcript) =>
      new HeddleBuildPlugin(relayBrowser, transcript)
