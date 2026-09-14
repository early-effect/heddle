package example

import java.nio.charset.StandardCharsets
import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.protocol.JsonRpc.*
import heddle.mcp.transport.Http
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

object ExampleMcpSpec extends ZIOSpecDefault:
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
      test("HTTP loopback discover list and get_users_id"):
        for
          store <- Ref.make(Map(1 -> User(1, "Ada")))
          mcp   <- ZIO.fromEither(Mcp.from(Main.publicApi(store)))
          http  <- Main.publicApi(store).routes(Request.get("/users/1"))
          disc  <- postMcp(mcp.routes, "server/discover", obj(), 1)
          list  <- postMcp(mcp.routes, "tools/list", obj(), 2)
          call  <- postMcp(
            mcp.routes,
            "tools/call",
            obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
            3,
            Some("get_users_id"),
          )
        yield assertTrue(
          http.status == Status.Ok,
          http.body.asString.contains("Ada"),
          disc.status == Status.Ok,
          disc.body.asString.contains("2026-07-28"),
          list.body.asString.contains("get_users_id"),
          call.status == Status.Ok,
          call.body.asString.contains("Ada"),
          !call.body.asString.contains("\"isError\":true"),
        )
      ,
      test("stdio pipe discover list and get_users_id"):
        val lines =
          List(
            mcpReq("server/discover", obj(), 1).toJson,
            mcpReq("tools/list", obj(), 2).toJson,
            mcpReq(
              "tools/call",
              obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
              3,
            ).toJson,
          ).mkString("", "\n", "\n")
        val in  = java.io.ByteArrayInputStream(lines.getBytes(StandardCharsets.UTF_8))
        val out = java.io.ByteArrayOutputStream()
        for
          store <- Ref.make(Map(1 -> User(1, "Ada")))
          mcp   <- ZIO.fromEither(Mcp.from(Main.publicApi(store)))
          _     <- mcp.stdio(in, out)
        yield
          val text = String(out.toByteArray, StandardCharsets.UTF_8)
          assertTrue(text.contains("2026-07-28"), text.contains("get_users_id"), text.contains("Ada"))
      ,
      test("subprocess --mcp-stdio discover list and get_users_id"):
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
                    obj("name" -> Json.Str("get_users_id"), "arguments" -> obj("id" -> Json.Num(1))),
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
              text.contains("get_users_id"),
              text.contains("Ada"),
            )
          },
    ) @@ TestAspect.timeout(30.seconds)
end ExampleMcpSpec
