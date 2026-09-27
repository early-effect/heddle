package example

import java.nio.charset.StandardCharsets
import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.client.{McpClient, McpError, McpStdio}
import heddle.mcp.protocol.{Era, Implementation, RequestMeta}
import heddle.mcp.transport.Http
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

/** Raw wire JSON on purpose, since this spec checks what a real MCP host sends and reads, plus heddle's own client. */
object ExampleMcpSpec extends ZIOSpecDefault:
  private def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields*)

  private val ProtocolVersion = heddle.mcp.protocol.ProtocolVersion.Current.value
  private val MetaVersion     = RequestMeta.VersionKey
  private val MetaClientCaps  = RequestMeta.ClientCapsKey

  private object Legacy:
    val ProtocolVersion = heddle.mcp.protocol.ProtocolVersion.Legacy.value
    val SessionHeader   = Http.SessionHeader

  private def mcpReq(method: String, params: Json.Obj, id: Int): Json.Obj =
    val meta = obj(MetaVersion -> Json.Str(ProtocolVersion), MetaClientCaps -> obj())
    val p    = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> p)

  private def postMcp(
      routes: Routes[Any, ?],
      method: String,
      params: Json.Obj,
      id: Int,
      name: Option[String] = None,
  ) =
    val body = mcpReq(method, params, id).toJson
    val req0 =
      Request
        .post("/mcp", Body.json(body))
        .withHeader(Http.ProtocolHeader, ProtocolVersion)
        .withHeader(Http.MethodHeader, method)
    val req = name.fold(req0)(n => req0.withHeader(Http.NameHeader, n))
    routes(req)
  end postMcp

  def spec =
    suite("example MCP")(
      test("HTTP loopback discover list and get_show"):
        for
          office <- BoxOffice.seed
          mcp    <- ZIO.fromEither(Mcp.from(Main.publicApi(office), Main.writeApi(office)))
          http   <- Main.publicApi(office).routes(Request.get("/shows/1"))
          disc   <- postMcp(mcp.routes, "server/discover", obj(), 1)
          list   <- postMcp(mcp.routes, "tools/list", obj(), 2)
          call   <- postMcp(
            mcp.routes,
            "tools/call",
            obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
            3,
            Some("get_show"),
          )
        yield
          val listed = list.body.text
          assertTrue(
            http.status == Status.Ok,
            http.body.text.is(_.some).contains("Evening bill"),
            disc.status == Status.Ok,
            disc.body.text.is(_.some).contains("2026-07-28"),
            listed.is(_.some).contains("get_show"),
            listed.is(_.some).contains("seat_the_party"),
            listed.is(_.some).contains("list_shows"),
            listed.is(_.some).contains("pickup"),
            !listed.is(_.some).contains("create_hold"),
            !listed.is(_.some).contains("create_order"),
            call.status == Status.Ok,
            call.body.text.is(_.some).contains("Evening bill"),
            !call.body.text.is(_.some).contains("\"isError\":true"),
          )
      ,
      test("HTTP 2025 initialize list and get_show"):
        def legacy(method: String, params: Json.Obj, id: Int): Json.Obj =
          obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> params)
        for
          office <- BoxOffice.seed
          mcp    <- ZIO.fromEither(Mcp.from(Main.publicApi(office), Main.writeApi(office)))
          init   <- mcp.routes(
            Request.post(
              "/mcp",
              Body.json(
                legacy(
                  "initialize",
                  obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
                  1,
                ).toJson
              ),
            )
          )
          sid = init.header(Legacy.SessionHeader)
          list <- mcp.routes(
            Request
              .post("/mcp", Body.json(legacy("tools/list", obj(), 2).toJson))
              .withHeader(Http.ProtocolHeader, Legacy.ProtocolVersion)
              .withHeader(Legacy.SessionHeader, sid.getOrElse(""))
          )
          call <- mcp.routes(
            Request
              .post(
                "/mcp",
                Body.json(
                  legacy(
                    "tools/call",
                    obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
                    3,
                  ).toJson
                ),
              )
              .withHeader(Http.ProtocolHeader, Legacy.ProtocolVersion)
              .withHeader(Legacy.SessionHeader, sid.getOrElse(""))
          )
        yield assertTrue(
          init.status == Status.Ok,
          init.body.text.is(_.some).contains("2025-11-25"),
          list.status == Status.Ok,
          list.body.text.is(_.some).contains("get_show"),
          !list.body.text.is(_.some).contains("resultType"),
          call.status == Status.Ok,
          call.body.text.is(_.some).contains("Evening bill"),
        )
        end for
      ,
      test("stdio 2025 initialize list and get_show"):
        def legacy(method: String, params: Json.Obj, id: Int): Json.Obj =
          obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> params)
        val lines =
          List(
            legacy(
              "initialize",
              obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
              1,
            ).toJson,
            legacy("tools/list", obj(), 2).toJson,
            legacy(
              "tools/call",
              obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
              3,
            ).toJson,
          ).mkString("", "\n", "\n")
        val in  = java.io.ByteArrayInputStream(lines.getBytes(StandardCharsets.UTF_8))
        val out = java.io.ByteArrayOutputStream()
        for
          office <- BoxOffice.seed
          mcp    <- ZIO.fromEither(Mcp.from(Main.publicApi(office), Main.writeApi(office)))
          _      <- mcp.stdio(in, out)
        yield
          val text = String(out.toByteArray, StandardCharsets.UTF_8)
          assertTrue(
            text.contains("2025-11-25"),
            text.contains("get_show"),
            text.contains("Evening bill"),
            !text.contains("resultType"),
          )
        end for
      ,
      test("stdio pipe discover list and get_show"):
        val lines =
          List(
            mcpReq("server/discover", obj(), 1).toJson,
            mcpReq("tools/list", obj(), 2).toJson,
            mcpReq(
              "tools/call",
              obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
              3,
            ).toJson,
          ).mkString("", "\n", "\n")
        val in  = java.io.ByteArrayInputStream(lines.getBytes(StandardCharsets.UTF_8))
        val out = java.io.ByteArrayOutputStream()
        for
          office <- BoxOffice.seed
          mcp    <- ZIO.fromEither(Mcp.from(Main.publicApi(office), Main.writeApi(office)))
          _      <- mcp.stdio(in, out)
        yield
          val text = String(out.toByteArray, StandardCharsets.UTF_8)
          assertTrue(text.contains("2026-07-28"), text.contains("get_show"), text.contains("Evening bill"))
      ,
      test("subprocess --mcp-stdio discover list and get_show"):
        ZIO
          .attemptBlocking {
            val javaHome = sys.props("java.home")
            val cp       =
              val here     = java.nio.file.Path.of("target", "mcp-stdio.classpath")
              val fromRoot = java.nio.file.Path.of("example", "target", "mcp-stdio.classpath")
              val file     = if java.nio.file.Files.isRegularFile(here) then here else fromRoot
              if java.nio.file.Files.isRegularFile(file) then java.nio.file.Files.readString(file).trim else "MISSING"
            val pb = ProcessBuilder(
              s"$javaHome/bin/java",
              "-cp",
              cp,
              "example.Main",
              "--mcp-stdio",
            )
            pb.redirectError(ProcessBuilder.Redirect.PIPE)
            val proc = pb.start()
            try
              val os      = proc.getOutputStream
              val payload =
                List(
                  mcpReq("server/discover", obj(), 1).toJson,
                  mcpReq("tools/list", obj(), 2).toJson,
                  mcpReq(
                    "tools/call",
                    obj("name" -> Json.Str("get_show"), "arguments" -> obj("id" -> Json.Num(1))),
                    3,
                  ).toJson,
                ).mkString("", "\n", "\n")
              os.write(payload.getBytes(StandardCharsets.UTF_8))
              os.flush()
              os.close()
              val text = String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
              proc.getErrorStream.readAllBytes()
              val code = proc.waitFor()
              (text, code)
            finally proc.destroyForcibly()
            end try
          }
          .map { (text, code) =>
            assertTrue(
              code == 0,
              text.contains("2026-07-28"),
              text.contains("get_show"),
              text.contains("Evening bill"),
            )
          }
      ,
      test("heddle's own client spawns the stdio server and makes typed calls with the shared endpoints"):
        val command =
          ChildCommand(s"${sys.props("java.home")}/bin/java", Chunk("-cp", classpath, "example.Main", "--mcp-stdio"))
        val client = McpClient.Settings(Implementation("example-spec", "0.0.1"))
        ZIO.scoped {
          for
            session <- McpStdio.spawn(command, client)
            tools   <- session.listTools
            show    <- session.call(Endpoints.getShow)(1)
            shows   <- session.call(Endpoints.listShows)(())
          yield assertTrue(
            session.era == Era.Stateless,
            tools.exists(_.name.value == "get_show"),
            show.title == "Evening bill",
            shows.exists(_.title == "Evening bill"),
          )
        }
      ,
      test("a program that does not exist is a Spawn error, not a hang"):
        val missing = ChildCommand("/no/such/mcp-server")
        ZIO.scoped(McpStdio.spawn(missing, McpClient.Settings(Implementation("spec", "0"))).flip).map { e =>
          assertTrue(e match
            case McpError.Spawn("/no/such/mcp-server", _) => true
            case _                                        => false)
        },
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)

  private def classpath: String =
    val here     = java.nio.file.Path.of("target", "mcp-stdio.classpath")
    val fromRoot = java.nio.file.Path.of("example", "target", "mcp-stdio.classpath")
    val file     = if java.nio.file.Files.isRegularFile(here) then here else fromRoot
    if java.nio.file.Files.isRegularFile(file) then java.nio.file.Files.readString(file).trim else "MISSING"
end ExampleMcpSpec
