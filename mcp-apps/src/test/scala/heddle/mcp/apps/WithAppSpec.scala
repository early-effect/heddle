package heddle.mcp.apps

import heddle.*
import heddle.error.HeddleError
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.client.{McpClient, McpError, McpSession}
import heddle.mcp.protocol.{ExtensionId, Implementation, ResourceContents}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

final case class Count(value: Int) derives Schema, JsonCodec

object WithAppSpec extends ZIOSpecDefault:
  private val show  = Endpoint.get("counter").out[Count].name("show_counter").summary("Open the counter")
  private val inc   = Endpoint.post("counter" / "inc").out[Count].name("inc")
  private val reset = Endpoint.post("counter" / "reset").out[Count].name("reset")

  private def api(n: Ref[Int]) =
    Api("Counter", "1.0.0")
      .job(show)(_ => n.get.map(Count(_)))
      .resource(inc)(_ => n.updateAndGet(_ + 1).map(Count(_)))
      .resource(reset)(_ => n.set(0).as(Count(0)))

  private val counter =
    Shed(UiUri("ui://counter/view"), "Counter", Grant.launch(show))((inc = Grant.app(inc), reset = Grant.app(reset)))

  private val document = UiDocument("Counter", "document.getElementById('app').textContent = 'hi'")

  private val mcp: IO[NonEmptyChunk[McpBuildError], Mcp[Any]] =
    Ref.make(0).flatMap(n => ZIO.fromEither(Mcp.from(api(n))))

  private def app(shed: Shed[?, ?, ?]): IO[NonEmptyChunk[HeddleError], Mcp[Any]] =
    mcp.flatMap(m => ZIO.fromEither(m.withApp(shed, document)))

  private def served(shed: Shed[?, ?, ?]): ZIO[Scope, NonEmptyChunk[HeddleError] | McpError, McpSession] =
    app(shed).flatMap { a =>
      McpClient
        .http("http://counter.test/mcp", McpClient.Settings(Implementation("host", "1")))
        .provideSome[Scope](Client.inMemory(a.routes))
    }

  def spec = suite("withApp")(
    test("every granted tool carries _meta.ui; tools only the view calls are listed as app-only"):
      ZIO.scoped {
        served(counter).flatMap(_.listTools).map { tools =>
          val ui = tools.map(t => t.name.value -> UiMeta.decodeTool(t.meta)._1).toMap
          assertTrue(
            ui.get("show_counter") == Some(ToolUi(Some(counter.uri), Visibility.ModelAndApp)),
            ui.get("inc") == Some(ToolUi(Some(counter.uri), Visibility.App)),
            ui.get("reset") == Some(ToolUi(Some(counter.uri), Visibility.App)),
          )
        }
      }
    ,
    test("the view resource is the rendered document, with the policy and the script hash on the content"):
      ZIO.scoped {
        served(counter.withPolicy(UiPolicy(border = Border.Visible))).flatMap { s =>
          (s.listResources <*> s.readResource(counter.uri.value)).map { (listed, contents) =>
            val body = contents.collectFirst { case t: ResourceContents.Text => t }
            val meta = body.flatMap(_.meta)
            assertTrue(
              listed.map(_.mimeType) == Chunk(Some(UiMeta.MimeType)),
              body.exists(_.text == document.html),
              body.flatMap(_.mimeType).contains(UiMeta.MimeType),
              UiMeta.decodeResource(meta)._1 == UiPolicy(border = Border.Visible),
              meta.flatMap(_.get(HeddleMetaKey)).exists(_.toJson.contains(document.scriptHash)),
            )
          }
        }
      }
    ,
    test("the server advertises the MCP Apps extension with the view MIME type"):
      for
        withView <- app(counter)
        out      <- withView.handle(
          heddle.mcp.protocol.Message
            .stateless(heddle.mcp.protocol.RequestId.Num(1), heddle.mcp.protocol.ClientRequest.Discover)
            .json
        )
      yield assertTrue(
        out.exists(_.toJson.contains(s""""${ExtensionId.Ui.value}":{"mimeTypes":["${UiMeta.MimeType}"]}"""))
      )
    ,
    test("a view tool the server does not have is a build error"):
      val missing = Endpoint.post("counter" / "double").out[Count].name("double")
      val shed    = Shed(UiUri("ui://counter/view"), "Counter", Grant.launch(show))((double = Grant.app(missing)))
      mcp.map { m =>
        assertTrue(
          m.withApp(shed, document)
            .left
            .exists(_.exists {
              case AppBuildError.Mcp(McpBuildError.NoSuchTool(name)) => name.value == "double"
              case _                                                 => false
            })
        )
      }
    ,
    test("a launch tool the model cannot call is a build error"):
      val shed = Shed(UiUri("ui://counter/view"), "Counter", Grant.app(show))((inc = Grant.app(inc)))
      mcp.map { m =>
        assertTrue(
          m.withApp(shed, document)
            .left
            .exists(_.exists {
              case AppBuildError.LaunchNotForModel("show_counter") => true
              case _                                               => false
            })
        )
      }
    ,
    test("a shed keeps its launch grant's own types, so a view can render the launch result typed"):
      val launch: Grant[Unit, Nothing, Count] = counter.launch
      assertTrue(launch.toolName == show.doc.toolName)
    ,
    test("a shed's tools must all be grants"):
      typeCheck("""Shed(UiUri("ui://c/v"), "C", Grant.launch(show))((inc = 42))""").map { r =>
        assertTrue(r.left.exists(_.contains("must be a Grant")))
      }
    ,
    test("a script that says </script cannot end its own element"):
      val tricky = UiDocument("x", "const s = '</script><img src=x onerror=alert(1)>'")
      assertTrue(!tricky.html.contains("</script><img"), tricky.html.contains("<\\/script><img")),
  ) @@ TestAspect.timeout(60.seconds)
end WithAppSpec
