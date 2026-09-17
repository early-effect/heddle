package heddle.docs

import heddle.*
import heddle.brotli.Brotli
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Http extends DocSpecSuite:

  def doc = page("HTTP")(
    md"""
The HTTP surface is the one you already know how to hold: `Request`, `Response`, `Routes`,
`Handler`, `Middleware`, `Server`, `Client`. Bodies on the wire are streams.
`Body.asString` / `asBytes` are for in-memory bodies; use `collect` otherwise. `maxBodyBytes` is a
cap, not a buffer.

If you are building a hub, start at [The hub](the-hub.html) and come back here for the knobs.
""",
    section("Routes and the path DSL")(
      md"""
`Method.GET / "users" / int("id")` is a `PathCodec`. `int`, `long`, `string`, `uuid`, and
`trailing` are the captures. Unmatched paths are 404; wrong method is 405.
""",
      exampleZIO {
        val routes = Routes(
          Method.GET / "users" / int("id") -> { (id: Int) =>
            ZIO.succeed(Response.text(id.toString))
          }
        )
        for
          ok   <- routes(Request.get("/users/3"))
          miss <- routes(Request.get("/users/ada"))
        yield (ok.body.asString, miss.status)
      }.assert { case (body, miss) =>
        assertTrue(body == "3", miss == Status.NotFound)
      },
      illustrationIO(Hub.Lives.pathPlay).live.withMountKey(InteractiveRegistry.PathPlayground),
    ),
    section("Middleware")(
      md"""
`@@` wraps **this** `Routes` value. `++` is try left, then right on 404/405.
Gzip, decompress, CORS, auth, and `Middleware.timeout` attach to whoever you wrap.
Global gzip is `all @@ Middleware.compress()`, which is still wrapping routes, not a
server flag. Partial gzip is `(api ++ files) @@ Middleware.compress() ++ sse`.

Why that split exists is on [What stays open](what-stays-open.html).
""",
      exampleZIO {
        val routes =
          Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestId()
        routes(Request.get("/x")).map { res =>
          res.header("X-Request-Id").exists(_.nonEmpty)
        }
      }.assert(hasId => assertTrue(hasId)),
      illustrationIO(Hub.Lives.middleware).live.withMountKey(InteractiveRegistry.MiddlewareStack),
    ),
    section("Server")(
      md"""
`Server.install` / `Server.serve` shift onto ZIO's Loom executor. Accept and connections are
ordinary ZIO fibers. TLS is a compose-time layer, not a protocol flag:
`Server.serve(app).provide(Server.Config.defaults, Tls.pem(certPem, keyPem))`.
ALPN offers `h2` and `http/1.1` on that bind. Idle, header, and connection caps live on
`Server.Config`. See [What stays open](what-stays-open.html).
""",
      exampleZIO {
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server
            .install(routes, Server.Config.default.copy(host = "127.0.0.1", port = 0))
            .flatMap { server =>
              server.port.flatMap { port =>
                Client.get(s"http://127.0.0.1:$port/health").map { res =>
                  (res.status, res.body.asString)
                }
              }
            }
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body == "ok")
      },
    ),
    section("Client, files, compression")(
      md"""
`Client.get` / `Client.batched` are the HTTP/1.1 client. One-shot helpers take
`Client.Config` (default named values, including connect and read timeouts).
A caller-given file is `Files.fromPath`.
Compression is `@@ Middleware.compress` (gzip in core). Add `heddle-brotli` and pass
`Brotli.compressor` when you want `br`. Incoming `Content-Encoding` is
`@@ Middleware.decompress(maxBytes = ...)`.
""",
      exampleValue {
        Brotli.encode(zio.Chunk.fromArray("hi".getBytes("UTF-8"))).nonEmpty
      }.assert(ok => assertTrue(ok)),
    ),
  )
end Http
