package heddle.build

import histos.*
import histos.plugin.*

/** Planned task contracts are independent of their invocation implementations. */
final case class TaskSpec(
    name: TaskName,
    inputs: List[DeclaredInput],
    outputs: List[DeclaredOutput],
    dependencies: List[TaskDep],
):
  def declared: DeclaredTask =
    DeclaredTask(name, Seal.Hermetic, Set.empty, inputs, outputs, dependencies)

object TaskSpec:
  val pluginId: PluginId = PluginId("rocks.earlyeffect.heddle-build")

  private val implementation = DeclaredInput.Directory(RepoPath("build-tooling/src/main/scala"))

  val relay: TaskSpec = TaskSpec(
    TaskName("relay-script"),
    List(implementation),
    List(DeclaredOutput.File(OutputName("relay-source"), RepoPath("target/generated/relay/RelayScript.scala"))),
    List(
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

  val browserAssets: TaskSpec = TaskSpec(
    TaskName("browser-assets"),
    List(implementation),
    List(
      DeclaredOutput.File(OutputName("counter-view"), RepoPath("target/generated/browser/counter-view.js")),
      DeclaredOutput.File(OutputName("reference-host"), RepoPath("target/generated/browser/reference-host.js")),
    ),
    List(
      TaskDep.Link(ProjectName("appsBrowserView"), None, Some(JsOptimize.Fast)),
      TaskDep.Link(ProjectName("appsBrowserHost"), None, Some(JsOptimize.Fast)),
      TaskDep.EmitScoped(
        ProjectName("appsBrowser"),
        SourceSet.Test,
        PlatformSelection.only(Jvm),
        EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")),
        OutputName("counter-view"),
      ),
      TaskDep.EmitScoped(
        ProjectName("appsBrowser"),
        SourceSet.Test,
        PlatformSelection.only(Jvm),
        EmittedContent.ResourceFile(ResourceName("apps-browser/reference-host.js")),
        OutputName("reference-host"),
      ),
    ),
  )

  val browserCheck: TaskSpec = TaskSpec(
    TaskName("browser-check"),
    List(implementation),
    Nil,
    List(TaskDep.Link(ProjectName("browser"), None, Some(JsOptimize.Full))),
  )

  val transcript: TaskSpec = TaskSpec(
    TaskName("docs-transcript"),
    List(implementation),
    List(
      DeclaredOutput.File(OutputName("transcript-source"), RepoPath("target/generated/docs/HarnessTranscript.scala"))
    ),
    List(
      TaskDep.CompileScoped(ProjectName("docs"), None, Some(Jvm), SourceSet.Main),
      TaskDep.EmitScoped(
        ProjectName("docs"),
        SourceSet.Test,
        PlatformSelection.only(Jvm),
        EmittedContent.SourceFile(ArtifactPath("heddle/docs/ui/HarnessTranscript.scala")),
        OutputName("transcript-source"),
      ),
      TaskDep.EmitScoped(
        ProjectName("docs"),
        SourceSet.Main,
        PlatformSelection.only(Js),
        EmittedContent.SourceFile(ArtifactPath("heddle/docs/ui/HarnessTranscript.scala")),
        OutputName("transcript-source"),
      ),
    ),
  )

  val relayBrowser: List[TaskSpec] = List(relay, browserAssets, browserCheck)
  val docs: List[TaskSpec]         = List(transcript)
end TaskSpec
