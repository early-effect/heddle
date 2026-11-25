package heddle.build

import histos.*
import histos.plugin.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import zio.*

final case class ExpectedBrowserLink(module: ProjectName, version: ScalaVersion, optimize: JsOptimize)

enum RelayBrowserError:
  case ExpectedNamedLinkFailure(expected: ExpectedBrowserLink, matches: Int)
  case Destination(error: PluginRunError)
  case Read(path: Path)
  case Write(path: Path)
  case Copy(source: Path, destination: Path)
  case HashUnavailable
  case NodeImports(path: Path, modules: List[String])
  case ModuleOutsideProduct(path: Path)
  case UnknownTask(id: PluginTaskId)

  def pluginError: PluginRunError = this match
    case ExpectedNamedLinkFailure(expected, matches) =>
      failed(
        s"expected one ${expected.module.value} JS ${expected.version.value} ${expected.optimize} product, found $matches"
      )
    case Destination(error)         => error
    case Read(path)                 => failed(s"could not read $path")
    case Write(path)                => failed(s"could not write $path")
    case Copy(source, destination)  => failed(s"could not copy $source to $destination")
    case HashUnavailable            => failed("SHA-256 is unavailable")
    case NodeImports(path, modules) => failed(s"browser module $path imports Node modules: ${modules.mkString(", ")}")
    case ModuleOutsideProduct(path) => failed(s"browser import escapes the linked product: $path")
    case UnknownTask(id)            => PluginRunError.Unknown(id)

  private def failed(message: String): PluginRunError =
    PluginDetail.from(message.filterNot(_.isControl)) match
      case Right(detail) => PluginRunError.Failed(detail)
      case Left(_)       => PluginRunError.Defect("heddle.build.RelayBrowserError")
end RelayBrowserError

/** Only invocation-owned files are written. LinkedScript already denotes the effective minified Full product. */
final class RelayBrowserLive(files: RelayBrowserFiles) extends RelayBrowserTasks:
  def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    execute(task, request).mapError(_.pluginError)

  private[build] def execute(task: TaskSpec, request: RunRequest): IO[RelayBrowserError, PluginOutput] =
    if request.id.plugin != TaskSpec.pluginId || request.id.name != task.name then
      ZIO.fail(RelayBrowserError.UnknownTask(request.id))
    else if task == TaskSpec.relay then relay(request)
    else if task == TaskSpec.browserAssets then assets(request)
    else if task == TaskSpec.browserCheck then browserCheck(request)
    else ZIO.fail(RelayBrowserError.UnknownTask(request.id))

  private def relay(request: RunRequest): IO[RelayBrowserError, PluginOutput] =
    for
      linked <- select(request, ProjectName("mcpAppsRelay"), JsOptimize.Full)
      text   <- files.read(linked.script)
      source <- RelaySource.generate(text)
      out    <- destination(request, OutputName("relay-source"))
      _      <- files.write(out, source.scalaSource)
    yield PluginOutput(
      "relay script and escaped-byte CSP hash generated",
      List(WrittenFile(OutputName("relay-source"), RepoPath("target/generated/relay/RelayScript.scala"))),
    )

  private def assets(request: RunRequest): IO[RelayBrowserError, PluginOutput] =
    for
      view    <- select(request, ProjectName("appsBrowserView"), JsOptimize.Fast)
      host    <- select(request, ProjectName("appsBrowserHost"), JsOptimize.Fast)
      viewOut <- destination(request, OutputName("counter-view"))
      hostOut <- destination(request, OutputName("reference-host"))
      _       <- files.copy(view.script, viewOut)
      _       <- files.copy(host.script, hostOut)
    yield PluginOutput(
      "browser Test assets copied",
      List(
        WrittenFile(OutputName("counter-view"), RepoPath("target/generated/browser/counter-view.js")),
        WrittenFile(OutputName("reference-host"), RepoPath("target/generated/browser/reference-host.js")),
      ),
    )

  private def browserCheck(request: RunRequest): IO[RelayBrowserError, PluginOutput] =
    for
      linked <- select(request, ProjectName("browser"), JsOptimize.Full)
      _      <- files.checkBrowser(linked.script)
    yield PluginOutput("Full browser ES module is Node-free", Nil)

  private[build] def select(
      request: RunRequest,
      module: ProjectName,
      optimize: JsOptimize,
  ): IO[RelayBrowserError, LinkedScript] =
    // LinkedScript is a JS-only product. No classpath, repository scan, or Fast fallback can satisfy this selector.
    val expected = ExpectedBrowserLink(module, Catalog.scala3, optimize)
    val matches  = request.linked.filter: linked =>
      linked.module == expected.module && linked.version == expected.version && linked.optimize == expected.optimize
    matches match
      case linked :: Nil => ZIO.succeed(linked)
      case _             => ZIO.fail(RelayBrowserError.ExpectedNamedLinkFailure(expected, matches.size))

  private def destination(request: RunRequest, name: OutputName): IO[RelayBrowserError, Path] =
    ZIO.fromEither(request.file(name)).mapError(RelayBrowserError.Destination(_))
end RelayBrowserLive

object RelayBrowserLive:
  val layer: ULayer[RelayBrowserTasks] = ZLayer.succeed(new RelayBrowserLive(new RelayBrowserFiles))

/** NIO failures stop at this adapter. It never discovers files by scanning a parent directory. */
final class RelayBrowserFiles:
  def read(path: Path): IO[RelayBrowserError, String] =
    ZIO.attemptBlockingInterrupt(Files.readString(path, UTF_8)).mapError(_ => RelayBrowserError.Read(path))

  def write(path: Path, text: String): IO[RelayBrowserError, Unit] =
    prepare(path) *> ZIO
      .attemptBlockingInterrupt(Files.writeString(path, text, UTF_8))
      .mapError(_ => RelayBrowserError.Write(path))
      .unit

  def copy(source: Path, destination: Path): IO[RelayBrowserError, Unit] =
    prepare(destination) *> ZIO
      .attemptBlockingInterrupt(Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING))
      .mapError(_ => RelayBrowserError.Copy(source, destination))
      .unit

  private def prepare(path: Path): IO[RelayBrowserError, Unit] =
    ZIO.foreachDiscard(Option(path.getParent)): parent =>
      ZIO.attemptBlockingInterrupt(Files.createDirectories(parent)).mapError(_ => RelayBrowserError.Write(path)).unit

  def checkBrowser(entry: Path): IO[RelayBrowserError, Unit] =
    val absolute = entry.toAbsolutePath.normalize
    val root     = Option(absolute.getParent).getOrElse(absolute)
    def visit(path: Path, seen: Set[Path]): IO[RelayBrowserError, Set[Path]] =
      if seen.contains(path) then ZIO.succeed(seen)
      else if !path.startsWith(root) then ZIO.fail(RelayBrowserError.ModuleOutsideProduct(path))
      else
        for
          code <- read(path)
          imports = BrowserImports.modules(code)
          nodes   = imports.filter(BrowserImports.isNode).distinct
          _            <- ZIO.fail(RelayBrowserError.NodeImports(path, nodes)).when(nodes.nonEmpty)
          dependencies <- ZIO.foreach(imports.filter(_.startsWith("."))): relative =>
            ZIO
              .attempt(path.resolveSibling(relative).normalize)
              .mapError(_ => RelayBrowserError.ModuleOutsideProduct(path))
          visited <- ZIO.foldLeft(dependencies)(seen + path)((done, dependency) => visit(dependency, done))
        yield visited
    visit(absolute, Set.empty).unit
end RelayBrowserFiles

private[build] object BrowserImports:
  private enum Token:
    case Word(value: String)
    case Quoted(value: String)
    case Symbol(value: Char)

  private val builtins = Set(
    "assert",
    "async_hooks",
    "buffer",
    "child_process",
    "cluster",
    "console",
    "constants",
    "crypto",
    "dgram",
    "diagnostics_channel",
    "dns",
    "domain",
    "events",
    "fs",
    "http",
    "http2",
    "https",
    "module",
    "net",
    "os",
    "path",
    "perf_hooks",
    "process",
    "punycode",
    "querystring",
    "readline",
    "repl",
    "sqlite",
    "stream",
    "string_decoder",
    "sys",
    "test",
    "timers",
    "tls",
    "trace_events",
    "tty",
    "url",
    "util",
    "v8",
    "vm",
    "wasi",
    "worker_threads",
    "zlib",
  )

  def isNode(module: String): Boolean = module.startsWith("node:") || builtins.contains(module.takeWhile(_ != '/'))

  def modules(code: String): List[String] =
    import Token.*
    def collect(tokens: List[Token]): List[String] = tokens match
      case Word("import") :: Quoted(value) :: rest                 => value :: collect(rest)
      case Word("from") :: Quoted(value) :: rest                   => value :: collect(rest)
      case Word("import") :: Symbol('(') :: Quoted(value) :: rest  => value :: collect(rest)
      case Word("require") :: Symbol('(') :: Quoted(value) :: rest => value :: collect(rest)
      case _ :: rest                                               => collect(rest)
      case Nil                                                     => Nil
    collect(tokenize(code.toList))

  private def tokenize(chars: List[Char]): List[Token] =
    import Token.*
    def hex(value: List[Char]): Option[Int] =
      value.foldLeft(Option(0)): (decoded, c) =>
        for
          total <- decoded
          digit <- Option(Character.digit(c, 16)).filter(_ >= 0)
        yield total * 16 + digit
    def quoted(rest: List[Char], quote: Char, value: List[Char]): (String, List[Char]) = rest match
      case '\\' :: 'u' :: a :: b :: c :: d :: tail =>
        hex(List(a, b, c, d)) match
          case Some(decoded) => quoted(tail, quote, decoded.toChar :: value)
          case None          => quoted('u' :: a :: b :: c :: d :: tail, quote, value)
      case '\\' :: 'x' :: a :: b :: tail =>
        hex(List(a, b)) match
          case Some(decoded) => quoted(tail, quote, decoded.toChar :: value)
          case None          => quoted('x' :: a :: b :: tail, quote, value)
      case '\\' :: c :: tail       => quoted(tail, quote, c :: value)
      case c :: tail if c == quote => (value.reverse.mkString, tail)
      case c :: tail               => quoted(tail, quote, c :: value)
      case Nil                     => (value.reverse.mkString, Nil)
    def block(rest: List[Char]): List[Char] = rest match
      case '*' :: '/' :: tail => tail
      case _ :: tail          => block(tail)
      case Nil                => Nil
    @ _root_.scala.annotation.tailrec
    def loop(remaining: List[Char], result: List[Token]): List[Token] = remaining match
      case '/' :: '/' :: rest                                             => loop(rest.dropWhile(_ != '\n'), result)
      case '/' :: '*' :: rest                                             => loop(block(rest), result)
      case quote :: rest if quote == '"' || quote == '\'' || quote == '`' =>
        val (value, tail) = quoted(rest, quote, Nil)
        loop(tail, Quoted(value) :: result)
      case c :: _ if c.isLetter || c == '_' || c == '$' =>
        val (word, rest) = remaining.span(c => c.isLetterOrDigit || c == '_' || c == '$')
        loop(rest, Word(word.mkString) :: result)
      case c :: rest if c.isWhitespace => loop(rest, result)
      case c :: rest                   => loop(rest, Symbol(c) :: result)
      case Nil                         => result.reverse
    loop(chars, Nil)
end BrowserImports
