package heddle.build

import histos.*
import histos.plugin.*
import zio.*
import zio.test.*

object TaskSpecSpec extends ZIOSpecDefault:
  def spec = suite("generator contracts")(
    test("relay emits one exact source to host Main on JVM and JS after the Full classic link") {
      assertTrue(
        TaskSpec.relay.outputs == List(
          DeclaredOutput.File(OutputName("relay-source"), RepoPath("target/generated/relay/RelayScript.scala"))
        ),
        TaskSpec.relay.dependencies == List(
          TaskDep.Link(ProjectName("mcpAppsRelay"), None, Some(JsOptimize.Full)),
          TaskDep.EmitScoped(
            ProjectName("mcpAppsHost"),
            SourceSet.Main,
            PlatformSelection.only(Jvm, Js),
            EmittedContent.SourceFile(ArtifactPath("heddle/mcp/apps/host/RelayScript.scala")),
            OutputName("relay-source"),
          ),
        ),
      )
    },
    test("browser assets are two named files, never a widened Test resource directory") {
      val resources = TaskSpec.browserAssets.dependencies.collect {
        case TaskDep.EmitScoped(module, scope, platforms, EmittedContent.ResourceFile(name), output) =>
          (module, scope, platforms, name, output)
      }
      assertTrue(
        TaskSpec.browserAssets.outputs.forall(_.kind == OutputKind.File),
        TaskSpec.browserAssets.outputs.size == 2,
        resources == List(
          (
            ProjectName("appsBrowser"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            ResourceName("apps-browser/counter-view.js"),
            OutputName("counter-view"),
          ),
          (
            ProjectName("appsBrowser"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            ResourceName("apps-browser/reference-host.js"),
            OutputName("reference-host"),
          ),
        ),
        TaskSpec.browserAssets.dependencies.collect { case link: TaskDep.Link => link.optimize } == List(
          Some(JsOptimize.Fast),
          Some(JsOptimize.Fast),
        ),
      )
    },
    test("transcript diamond consumes JVM Main and emits only JVM Test and JS Main") {
      val stages = TaskSpec.transcript.dependencies.collect {
        case TaskDep.EmitScoped(module, scope, platforms, _, output) => (module, scope, platforms, output)
      }
      assertTrue(
        TaskSpec.transcript.dependencies
          .contains(TaskDep.CompileScoped(ProjectName("docs"), None, Some(Jvm), SourceSet.Main)),
        stages == List(
          (ProjectName("docs"), SourceSet.Test, PlatformSelection.only(Jvm), OutputName("transcript-source")),
          (ProjectName("docs"), SourceSet.Main, PlatformSelection.only(Js), OutputName("transcript-source")),
        ),
        TaskSpec.transcript.outputs.size == 1,
      )
    },
    test("browser check requires Full link even when it produces no file") {
      assertTrue(
        TaskSpec.browserCheck.outputs.isEmpty,
        TaskSpec.browserCheck.dependencies == List(TaskDep.Link(ProjectName("browser"), None, Some(JsOptimize.Full))),
      )
    },
    test("wired plugin dispatches to both live services and preserves their typed failures") {
      val plugin = ZIO
        .service[HeddleBuildPlugin]
        .provideLayer(
          (RelayBrowserLive.layer ++ TranscriptLive.layer) >>> HeddleBuildPlugin.layer
        )
      ZIO.scoped:
        for
          scope  <- ZIO.scope
          runner <- plugin
          first  <- runner
            .run(request(TaskSpec.browserAssets.name))
            .exit
            .provideSomeLayer[Scope](PluginScopes.live(scope))
          second <- runner
            .run(request(TaskSpec.transcript.name))
            .exit
            .provideSomeLayer[Scope](PluginScopes.live(scope))
        yield assertTrue(
          first.causeOption
            .flatMap(_.failureOption)
            .contains(
              RelayBrowserError
                .ExpectedNamedLinkFailure(
                  ExpectedBrowserLink(ProjectName("appsBrowserView"), Catalog.scala3, JsOptimize.Fast),
                  0,
                )
                .pluginError
            ),
          second.causeOption
            .flatMap(_.failureOption)
            .contains(
              PluginRunError.MissingDestination(OutputName("transcript-source"))
            ),
        )
    },
  )

  private def request(task: TaskName): RunRequest = RunRequest(
    PluginTaskId(TaskSpec.pluginId, task),
    java.nio.file.Path.of("."),
    Nil,
    PluginArgs.empty,
    Nil,
    InvocationContext(
      InvocationId.derive("foundation"),
      ClientId("foundation"),
      CommandIdentity.Task(PluginTaskId(TaskSpec.pluginId, task)),
      histos.lock.ExecutionKind.Local,
      Interaction.Unattended,
      OfflineSelection.Offline,
    ),
    PluginEvents.closedInvocation,
  )
end TaskSpecSpec
