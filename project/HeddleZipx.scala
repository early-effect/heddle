import sbt.*
import zipx.plugin.ZipxPlugin.autoImport.*
import zipx.shell.Exec as ZipxExec

/** Platform Verify: JVM test stays the default `test` job; JS and Native are sibling Once jobs. */
object HeddleZipx:

  private val javaOpts = Map(
    "JAVA_OPTS" -> EnvValue.plain(
      "-Xms2048M -Xmx2048M -Xss6M -XX:ReservedCodeCacheSize=256M -Dfile.encoding=UTF-8"
    )
  )

  private val TestJs     = CapabilityName("test-js")
  private val TestNative = CapabilityName("test-native")

  private def alias(name: String): SbtCommand =
    SbtCommand.raw(name).fold(msg => sys.error(s"zipx: $msg"), identity)

  private def aptInstall(packages: Word*): Script =
    Script(
      ZipxExec("sudo", Word.lit("apt-get"), Word.lit("update")) &&
        ZipxExec.of(
          "sudo",
          List(Word.lit("apt-get"), Word.lit("install"), Word.lit("-y")) ++ packages.toList,
        )
    )

  private val nativeCiSetup: Steps = Steps.built("heddle-native-ci")(
    Step
      .run(
        aptInstall(
          Word.lit("clang"),
          Word.lit("libstdc++-12-dev"),
          Word.lit("libgc-dev"),
          Word.lit("libunwind-dev"),
          Word.lit("libssl-dev"),
        )
      )
      .named("Install Scala Native build dependencies")
  )

  private val upstream = JobCondition.repositoryIs("early-effect/heddle")

  def settings: Seq[Setting[?]] = Seq(
    zipxJavaVersion      := JdkVersion("25"),
    zipxWorkflowDispatch := true,
    zipxEnv              := javaOpts,
    zipxCapabilities ++= Seq(
      Capability.once(
        name = Capability.TestName,
        command = alias("testJVM"),
        env = javaOpts,
      ),
      Capability.once(
        name = TestJs,
        command = alias("testJS"),
        env = javaOpts,
      ),
      Capability.once(
        name = TestNative,
        command = alias("testNative"),
        extraSteps = nativeCiSetup,
        env = javaOpts,
      ),
      ZipxCentral.release.withCondition(upstream),
      ZipxDocs.pages().andCondition(upstream),
    ),
  )
end HeddleZipx
