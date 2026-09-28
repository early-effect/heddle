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

  private val TestJs       = CapabilityName("test-js")
  private val TestNative   = CapabilityName("test-native")
  private val TestBrowsers = CapabilityName("test-browsers")

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

  /** The pinned browsers (`chekhovInstall`), then the Linux libraries they need (`install-deps`). */
  private val browserCiSetup: Steps = Steps.built("heddle-browser-ci")(
    // sbt-chekhov is enabled on appsBrowser alone, so its install task is scoped there.
    Step
      .run(Script(ZipxExec("sbt", Word.lit("appsBrowser/chekhovInstall"))))
      .named("Install the pinned Playwright browsers"),
    Step
      .run(Script(ZipxExec("bash", Word.lit("scripts/install-browser-deps.sh"))))
      .named("Install their Linux system libraries"),
  )

  private val upstream = JobCondition.repositoryIs("early-effect/heddle")

  def settings: Seq[Setting[?]] = Seq(
    zipxJavaVersion      := JdkVersion("25"),
    zipxWorkflowDispatch := true,
    zipxEnv              := javaOpts,
    zipxCapabilities ++= Seq(
      // Replaces the builtin test by name, so it has to claim the LocalDir snapshot itself.
      Capability
        .once(
          name = Capability.TestName,
          command = alias("testJVM"),
          env = javaOpts,
        )
        .withLocalCache(LocalCacheMode.Save),
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
      // Real Chromium, Firefox, and WebKit: what the MCP Apps sandbox assumes of each (apps-browser).
      Capability.once(
        name = TestBrowsers,
        command = alias("testBrowsers"),
        extraSteps = browserCiSetup,
        env = javaOpts,
      ),
      ZipxCentral.release.withCondition(upstream),
      ZipxDocs.pages().andCondition(upstream),
    ),
  )
end HeddleZipx
