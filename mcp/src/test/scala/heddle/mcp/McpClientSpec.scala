package heddle.mcp

import heddle.*
import heddle.mcp.client.{McpCallFailure, McpClient, McpError, McpSession}
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object McpClientSpec extends ZIOSpecDefault:
  private val getItem = Endpoint.get("items" / int("id")).out[Item].outError[String](Status.NotFound).mcp

  private val greet = Endpoint.get("greet" / string("who")).out[String].mcp

  private val api = Api("Shop", "1.0.0")
    .bind(getItem)(id => if id == 1 then ZIO.succeed(Item(1, "ada")) else ZIO.fail(s"no item $id"))
    .bind(greet)(who => ZIO.succeed(s"hi $who"))

  private val board = ServedResource.text(Resource("ui://shop/board", "board"), "<p>hi</p>")

  private val server: Mcp[Any] =
    Mcp.from(api).flatMap(_.withResources(board)).toOption.get

  private val settings = McpClient.Settings(Implementation("spec", "0.0.1"))

  private def overHttp(routes: Routes[Any, Response]): ZIO[Scope, McpError, McpSession] =
    McpClient.http("http://mcp.test/mcp", settings).provideSome[Scope](Client.inMemory(routes))

  /** A 2025-only server: it does not know `server/discover`. */
  private val sessionOnly: Routes[Any, Nothing] =
    Routes.fromHandler(Handler { (req: Request) =>
      req.body.utf8.orDie.flatMap { raw =>
        val decoded = raw.fromJson[Json].toOption.flatMap(Message.decode(_).toOption)
        decoded match
          case Some(Message.Request(id, Methods.Discover, _)) =>
            val err = Message.Error(Some(id), RpcError.methodNotFound(Methods.Discover))
            ZIO.succeed(Response.json(err.json.toJson, Status.NotFound))
          case _ => server.routes(req.copy(body = Body.json(raw)))
      }
    })

  /** The client end of a pipe whose other end is `serve`. */
  private def overPipe(serve: LinePipe => UIO[Unit]): ZIO[Scope, McpError, McpSession] =
    LinePipe.connected.flatMap { (client, other) =>
      serve(other).forkScoped *> McpClient.pipe(client, settings)
    }

  /** A scripted peer: reads each line and answers with what `reply` returns for it. */
  private def peer(reply: Json.Obj => List[Json.Obj]): LinePipe => UIO[Unit] =
    pipe =>
      pipe.readLine.orDie.flatMap {
        case None       => ZIO.unit
        case Some(line) =>
          val msgs = line.fromJson[Json.Obj].toOption.toList.flatMap(reply)
          ZIO.foreachDiscard(msgs)(m => pipe.writeLine(m.toJson).orDie)
      }.forever

  private def idOf(msg: Json.Obj): Json = msg.get("id").getOrElse(Json.Null)

  private def resultFor(msg: Json.Obj, result: Json.Obj): Json.Obj =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> idOf(msg), "result" -> result)

  def spec = suite("McpClient")(
    suite("over HTTP")(
      test("negotiates 2026-07-28 and learns who the server is"):
        ZIO.scoped(
          overHttp(server.routes).map(s => assertTrue(s.era == Era.Stateless, s.server.exists(_.name == "Shop")))
        )
      ,
      test("falls back to a 2025-11-25 session when the server does not know server/discover"):
        ZIO.scoped {
          overHttp(sessionOnly).flatMap { s =>
            s.listTools.map(tools => assertTrue(s.era == Era.Session, tools.exists(_.name.value == "get_items_id")))
          }
        }
      ,
      test("lists tools and resources, and reads a resource"):
        ZIO.scoped {
          for
            s         <- overHttp(server.routes)
            tools     <- s.listTools
            resources <- s.listResources
            read      <- s.readResource("ui://shop/board")
          yield assertTrue(
            tools.map(_.name.value).toSet == Set("get_items_id", "get_greet_who"),
            resources.map(_.uri) == Chunk("ui://shop/board"),
            read == Chunk(ResourceContents.Text("ui://shop/board", None, "<p>hi</p>", None)),
          )
        }
      ,
      test("a typed call returns the endpoint's output, a wrapped one included"):
        ZIO.scoped {
          for
            s    <- overHttp(server.routes)
            item <- s.call(getItem)(1)
            hi   <- s.call(greet)("ada")
          yield assertTrue(item == Item(1, "ada"), hi == "hi ada")
        }
      ,
      test("a typed call's declared error comes back as Domain"):
        ZIO.scoped {
          overHttp(server.routes).flatMap(_.call(getItem)(9).flip).map { failure =>
            assertTrue(failure == McpCallFailure.Domain("no item 9"))
          }
        }
      ,
      test("an unknown uri is a typed JSON-RPC error"):
        ZIO.scoped {
          overHttp(server.routes).flatMap(_.readResource("ui://shop/nope").flip).map { e =>
            assertTrue(e match
              case McpError.Rpc(_: RpcError.ResourceNotFound) => true
              case _                                          => false)
          }
        },
    ),
    suite("over a pipe")(
      test("the same server answers over stdio framing"):
        ZIO.scoped {
          for
            s    <- overPipe(end => server.stdio(end).orDie)
            item <- s.call(getItem)(1)
          yield assertTrue(s.era == Era.Stateless, item == Item(1, "ada"))
        }
      ,
      test("concurrent calls each get their own answer"):
        ZIO.scoped {
          overPipe(end => server.stdio(end).orDie).flatMap { s =>
            ZIO.foreachPar(Chunk.range(0, 24))(n => s.call(greet)(s"n$n").map(n -> _)).map { out =>
              assertTrue(out.forall((n, hi) => hi == s"hi n$n"))
            }
          }
        }
      ,
      test("answers returned in any order reach their own caller"):
        check(Gen.int(2, 16)) { n =>
          ZIO.scoped {
            val shuffler: LinePipe => UIO[Unit] = pipe =>
              for
                disc <- pipe.readLine.orDie.some.orDieWith(_ => RuntimeException("no discover"))
                _    <- pipe.writeLine(resultFor(disc.fromJson[Json.Obj].toOption.get, Json.Obj()).toJson).orDie
                reqs <- ZIO.foreach(Chunk.range(0, n))(_ =>
                  pipe.readLine.orDie.map(_.flatMap(_.fromJson[Json.Obj].toOption))
                )
                mixed <- Random.shuffle(reqs.flatten.toList)
                _     <- ZIO.foreachDiscard(mixed) { m =>
                  val args = m.get("params").flatMap(_.asObject).flatMap(_.get("arguments")).getOrElse(Json.Obj())
                  pipe
                    .writeLine(resultFor(m, Json.Obj("content" -> Json.Arr(), "structuredContent" -> args)).toJson)
                    .orDie
                }
              yield ()
            overPipe(shuffler).flatMap { s =>
              ZIO
                .foreachPar(Chunk.range(0, n))(i =>
                  s.callTool(ToolName("echo"), Json.Obj("i" -> Json.Num(i))).map(r => i -> r.structuredContent)
                )
                .map(out => assertTrue(out.forall((i, sc) => sc.contains(Json.Obj("i" -> Json.Num(i))))))
            }
          }
        }
      ,
      test("garbage and answers to other ids are ignored"):
        val noisy = peer { msg =>
          val real =
            resultFor(msg, Json.Obj("structuredContent" -> Json.Obj("ok" -> Json.Bool(true)), "content" -> Json.Arr()))
          List(
            Json.Obj("not"     -> Json.Str("json-rpc")),
            Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(987654), "result" -> Json.Obj()),
            real,
          )
        }
        ZIO.scoped {
          overPipe(noisy).flatMap(_.callTool(ToolName("x"), Json.Obj())).map { r =>
            assertTrue(r.structuredContent.contains(Json.Obj("ok" -> Json.Bool(true))))
          }
        }
      ,
      test("a server that hangs up fails waiting requests with Closed"):
        val hangUp: LinePipe => UIO[Unit] = pipe =>
          pipe.readLine.orDie *> (pipe match
            case c: LinePipe.Closable => c.close
            case _                    => ZIO.unit)
        ZIO.scoped(overPipe(hangUp).flip.map(e => assertTrue(e == McpError.Closed)))
      ,
      test("a request with no answer times out on the session's clock"):
        val silent: LinePipe => UIO[Unit] = pipe => pipe.readLine.orDie.forever
        ZIO.scoped {
          for
            fiber <- overPipe(silent).flip.fork
            _     <- TestClock.adjust(31.seconds)
            e     <- fiber.join
          yield assertTrue(e == McpError.TimedOut("server/discover"))
        }
      ,
      test("a server's ping is answered, and its other requests are refused"):
        ZIO.scoped {
          for
            seen <- Queue.unbounded[Json.Obj]
            asks = peer { msg =>
              msg.get("method") match
                case Some(Json.Str("server/discover")) =>
                  List(
                    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Str("s1"), "method" -> Json.Str("ping")),
                    Json.Obj(
                      "jsonrpc" -> Json.Str("2.0"),
                      "id"      -> Json.Str("s2"),
                      "method"  -> Json.Str("sampling/createMessage"),
                    ),
                    resultFor(msg, Json.Obj()),
                  )
                case _ => Nil
            }
            tap = (pipe: LinePipe) =>
              new LinePipe:
                def readLine =
                  pipe.readLine.tap(l => ZIO.foreachDiscard(l.flatMap(_.fromJson[Json.Obj].toOption))(seen.offer))
                def writeLine(line: String) = pipe.writeLine(line)
            _       <- overPipe(end => asks(tap(end)))
            replies <- seen.takeN(3)
          yield
            val byId = replies.flatMap(r => r.get("id").collect { case Json.Str(s) => s -> r }).toMap
            assertTrue(
              byId.get("s1").exists(_.get("result").isDefined),
              byId.get("s2").flatMap(_.get("error")).exists(_.toJson.contains("-32601")),
            )
        },
    ),
  ) @@ TestAspect.timeout(60.seconds)
end McpClientSpec
