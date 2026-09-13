package heddle

import zio.*
import zio.test.*

object MiddlewareSpec extends ZIOSpecDefault:
  def spec =
    suite("Middleware")(
      suite("identity")(
        test("leaves the handler response unchanged"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.identity
          routes(Request.get("/x")).map { res =>
            assertTrue(res.status == Status.Ok, res.body.asString == "ok")
          }
      ),
      suite("requestId")(
        test("adds a response header"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestId()
          routes(Request.get("/x")).map { res =>
            assertTrue(res.header("X-Request-Id").exists(_.nonEmpty))
          }
        ,
        test("reuses the incoming header"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestId()
          routes(Request.get("/x").withHeader("X-Request-Id", "abc")).map { res =>
            assertTrue(res.header("X-Request-Id").contains("abc"))
          }
        ,
        test("uses a custom header name"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestId("X-Corr")
          routes(Request.get("/x").withHeader("X-Corr", "n-1")).map { res =>
            assertTrue(res.header("X-Corr").contains("n-1"), res.header("X-Request-Id").isEmpty)
          }
        ,
        test("forwards the id to the handler"):
          val routes =
            Routes(
              Method.GET / "x" -> handler { (req: Request) =>
                ZIO.succeed(Response.text(req.header("X-Request-Id").getOrElse("")))
              }
            ) @@ Middleware.requestId()
          routes(Request.get("/x").withHeader("X-Request-Id", "abc")).map { res =>
            assertTrue(res.body.asString == "abc")
          },
      ),
      suite("cors")(
        test("adds Access-Control-Allow-Origin from the request Origin"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.cors()
          routes(Request.get("/x").withHeader("Origin", "http://localhost")).map { res =>
            assertTrue(res.header("Access-Control-Allow-Origin").contains("http://localhost"))
          }
        ,
        test("answers OPTIONS preflight"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.cors()
          routes(Request(Method.OPTIONS, Url.parse("/x")).withHeader("Origin", "http://localhost")).map { res =>
            assertTrue(
              res.status == Status.NoContent,
              res.header("Access-Control-Allow-Methods").exists(_.contains("GET")),
            )
          }
        ,
        test("echoes credentials when configured"):
          val cors   = Middleware.cors(Middleware.CorsConfig(allowOrigin = "http://localhost", allowCredentials = true))
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ cors
          routes(Request.get("/x").withHeader("Origin", "http://localhost")).map { res =>
            assertTrue(
              res.header("Access-Control-Allow-Origin").contains("http://localhost"),
              res.header("Access-Control-Allow-Credentials").contains("true"),
            )
          }
        ,
        test("falls back to * when Origin is missing"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.cors()
          routes(Request.get("/x")).map { res =>
            assertTrue(res.header("Access-Control-Allow-Origin").contains("*"))
          },
      ),
      suite("intercept")(
        test("rewrites the request before dispatch"):
          val routes =
            Routes(Method.GET / "b" -> Handler.text("b")) @@
              Middleware.intercept(req => req.copy(url = Url.parse("/b")))
          routes(Request.get("/a")).map { res =>
            assertTrue(res.body.asString == "b")
          }
        ,
        test("interceptZIO can reject with a response"):
          val routes =
            Routes(Method.GET / "x" -> Handler.text("ok")) @@
              Middleware.interceptZIO(_ => ZIO.fail(Response.badRequest("no")))
          routes(Request.get("/x")).map { res =>
            assertTrue(res.status == Status.BadRequest, res.body.asString == "no")
          },
      ),
      suite("mapResponse")(
        test("transforms the response"):
          val routes =
            Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.mapResponse(_.addHeader("X-M", "1"))
          routes(Request.get("/x")).map { res =>
            assertTrue(res.header("X-M").contains("1"))
          }
        ,
        test("mapResponseZIO transforms effectfully"):
          val routes =
            Routes(Method.GET / "x" -> Handler.text("ok")) @@
              Middleware.mapResponseZIO(res => ZIO.succeed(res.addHeader("X-Z", "z")))
          routes(Request.get("/x")).map { res =>
            assertTrue(res.header("X-Z").contains("z"))
          },
      ),
      suite("timeout")(
        test("returns 504 when the handler exceeds the limit"):
          val routes =
            Routes(Method.GET / "slow" -> handler(ZIO.sleep(1.hour).as(Response.text("late")))) @@
              Middleware.timeout(1.second)
          for
            fiber <- routes(Request.get("/slow")).fork
            _     <- TestClock.adjust(1.second)
            res   <- fiber.join
          yield assertTrue(res.status == Status.GatewayTimeout)
        ,
        test("does not fire when the handler completes in time"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.timeout(1.second)
          routes(Request.get("/x")).map { res =>
            assertTrue(res.status == Status.Ok, res.body.asString == "ok")
          },
      ),
      suite("provided")(
        test("injects a request-derived environment"):
          val authed =
            Routes(Method.GET / "me" -> handler(ZIO.serviceWith[User](u => Response.text(u.name))))
          val routes = authed.provided(_ => ZIO.succeed(User("ada")))
          routes(Request.get("/me")).map { res =>
            assertTrue(res.body.asString == "ada")
          }
        ,
        test("turns extraction failure into the error response"):
          val authed =
            Routes(Method.GET / "me" -> handler(ZIO.serviceWith[User](u => Response.text(u.name))))
          val routes = authed.provided(_ => ZIO.fail(Response.empty(Status.Unauthorized)))
          routes(Request.get("/me")).map { res =>
            assertTrue(res.status == Status.Unauthorized)
          },
      ),
      suite("debug")(
        test("still returns the handler response"):
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.debug
          routes(Request.get("/x")).map { res =>
            assertTrue(res.status == Status.Ok, res.body.asString == "ok")
          }
      ),
      suite("composition")(
        test("++ applies left then right"):
          val m =
            Middleware.mapResponse(_.addHeader("X-A", "1")) ++
              Middleware.mapResponse(_.addHeader("X-B", "2"))
          val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ m
          routes(Request.get("/x")).map { res =>
            assertTrue(res.header("X-A").contains("1"), res.header("X-B").contains("2"))
          }
        ,
        test("@@ after ++ wraps every route"):
          val routes =
            (Routes(Method.GET / "a" -> Handler.text("A")) ++
              Routes(Method.GET / "b" -> Handler.text("B"))) @@
              Middleware.mapResponse(_.addHeader("X-M", "1"))
          for
            a <- routes(Request.get("/a"))
            b <- routes(Request.get("/b"))
          yield assertTrue(a.header("X-M").contains("1"), b.header("X-M").contains("1")),
      ),
      suite("compress")(
        test("decompress recovers a gzip request body"):
          val raw    = "hello gzip"
          val enc    = Compressor.gzip.compress(Chunk.fromArray(raw.getBytes))
          val routes =
            Routes(
              Method.POST / "echo" -> Handler.fromFunctionZIO((req: Request) =>
                ZIO.succeed(Response.text(req.body.asString))
              )
            ) @@ Middleware.decompress()
          val req = Request.post("/echo", Body.fromBytes(enc)).withHeader("Content-Encoding", "gzip")
          routes(req).map(res => assertTrue(res.body.asString == raw))
        ,
        test("gzip JSON when Accept-Encoding offers gzip"):
          val body   = "n" * 2048
          val routes =
            Routes(Method.GET / "j" -> Handler.text(body)) @@ Middleware.compress(minBytes = 16)
          routes(Request.get("/j").withHeader("Accept-Encoding", "gzip")).map { res =>
            val raw = res.body.asBytes
            val out = Compressor.gunzip(raw)
            assertTrue(
              res.header("Content-Encoding").contains("gzip"),
              out.toArray.toSeq == body.getBytes.toSeq,
            )
          }
        ,
        test("identity stays uncompressed"):
          val routes =
            Routes(Method.GET / "j" -> Handler.text("n" * 2048)) @@ Middleware.compress(minBytes = 16)
          routes(Request.get("/j").withHeader("Accept-Encoding", "identity")).map { res =>
            assertTrue(res.header("Content-Encoding").isEmpty, res.body.asString == "n" * 2048)
          }
        ,
        test("no Accept-Encoding stays uncompressed"):
          val routes =
            Routes(Method.GET / "j" -> Handler.text("n" * 2048)) @@ Middleware.compress(minBytes = 16)
          routes(Request.get("/j")).map { res =>
            assertTrue(res.header("Content-Encoding").isEmpty)
          }
        ,
        test("skips text/event-stream"):
          val routes =
            Routes(
              Method.GET / "sse" -> handler(
                ZIO.succeed(heddle.sse.Sse.response(zio.stream.ZStream(heddle.sse.ServerSentEvent("x"))))
              )
            ) @@ Middleware.compress(minBytes = 0)
          routes(Request.get("/sse").withHeader("Accept-Encoding", "gzip")).map { res =>
            assertTrue(
              res.header("Content-Type").exists(_.startsWith("text/event-stream")),
              res.header("Content-Encoding").isEmpty,
            )
          }
        ,
        test("@@ subset of ++ only compresses wrapped routes"):
          val compressed =
            Routes(Method.GET / "a" -> Handler.text("a" * 2048)) @@ Middleware.compress(minBytes = 16)
          val plain  = Routes(Method.GET / "b" -> Handler.text("b" * 2048))
          val routes = compressed ++ plain
          for
            a <- routes(Request.get("/a").withHeader("Accept-Encoding", "gzip"))
            b <- routes(Request.get("/b").withHeader("Accept-Encoding", "gzip"))
          yield assertTrue(
            a.header("Content-Encoding").contains("gzip"),
            b.header("Content-Encoding").isEmpty,
          ),
      ),
    ) @@ TestAspect.timeout(5.seconds)

  final case class User(name: String)
end MiddlewareSpec
