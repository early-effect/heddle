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
`Handler`, `Middleware`, `Server`, `Client`. Bodies on the wire are streams. `body.utf8` and
`body.collect` read any body. `body.text` and `body.strict` are `Some` only for a body already in
memory, and `None` for a stream, never a throw. `maxBodyBytes` is a cap, not a buffer.

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
          body <- ok.body.utf8
          miss <- routes(Request.get("/users/ada"))
        yield (body, miss.status)
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
`Server.install` / `Server.serve` are ordinary ZIO fibers on every platform. JVM accept
defaults to Loom (`JvmScheduler.Loom`); `JvmScheduler.Default` still binds. JS is Node
`net` / `tls`. Native is POSIX sockets plus OpenSSL. TLS is a compose-time layer, not a
protocol flag: `Server.serve(app).provide(Server.Config.defaults, Tls.pem(certPem, keyPem))`.
HTTP/2 (ALPN `h2`) is the JVM bind. JS and Native serve HTTP/1.1 on that same `Routes`
value. Idle, header, and connection caps live on `Server.Config`. See
[What stays open](what-stays-open.html).
""",
      exampleZIO {
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server
            .install(routes, Server.Config.default.copy(host = "127.0.0.1", port = 0))
            .flatMap { server =>
              server.port.flatMap { port =>
                Client.get(s"http://127.0.0.1:$port/health").flatMap { res =>
                  res.body.utf8.map((res.status, _))
                }
              }
            }
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body == "ok")
      },
    ),
    section("Client")(
      md"""
`Client.batched` sends a request and reads the whole body. `Client.streaming` returns once the
head arrives and reads the body from the connection until its `Scope` closes. `Client.sse` is a
`text/event-stream` GET. All of them fail with `ClientError`, a closed enum of transport facts:
`InvalidTarget`, `Connect`, `ConnectTimeout`, `Tls`, `ReadTimeout`, `Io`, `Protocol`,
`PoolExhausted`, `InvalidTrust`. A response with any status is a success.

JVM and Native share one pooled HTTP/1.1 client. Every exchange settles its connection on
success, failure, and interruption, so a timed-out call gives its slot back. A close-delimited
body never returns to the pool. A GET, HEAD, PUT, DELETE, OPTIONS, or TRACE whose pooled
connection the server already dropped is sent once more on a fresh one; a POST is not. JS is
Node's `fetch`. TLS verifies the peer and its name on every platform. `ClientTls.trusting(pem)`
(JVM, Native) pins a private CA; Node reads `NODE_EXTRA_CA_CERTS`.
""",
      exampleZIO {
        ZIO.attemptBlocking(java.net.ServerSocket(0)).flatMap { ss =>
          val port = ss.getLocalPort
          ss.close()
          Client.get(s"http://127.0.0.1:$port/").either
        }
      }.assert { out =>
        assertTrue(out match
          case Left(ClientError.Connect(Authority("127.0.0.1", _), _)) => true
          case _                                                       => false)
      },
      md"""
`Client.call(endpoint)(in)` is the typed call. It fails with `CallFailure[E]`: `Domain(e)` for
one of the endpoint's own errors, `Transport` for a `ClientError`, `Undecodable` for a body the
codecs reject, `Unexpected` for a status the endpoint never declared. Nothing becomes a defect.
""",
      exampleZIO {
        val ep  = Endpoint.get("hello" / string("name")).out[String].outError[String](Status.NotFound)
        val api =
          Api("hello", "1").bind(ep)(name => if name == "ada" then ZIO.succeed(s"hi $name") else ZIO.fail("nobody"))
        (Client.call(ep)("ada") <*> Client.call(ep)("bob").either).provideLayer(Client.inMemory(api.routes))
      }.assert { case (ok, missing) =>
        assertTrue(ok == "hi ada", missing == Left(CallFailure.Domain("nobody")))
      },
    ),
    section("Files and compression")(
      md"""
A caller-given file is `Files.fromPath` (jailed under `SafePath` for directory roots).
Compression is `@@ Middleware.compress` (gzip in core). Add `heddle-brotli` and pass
`Brotli.compressor` when you want `br`. Incoming `Content-Encoding` is
`@@ Middleware.decompress(maxBytes = ...)`. Inflation stops at `maxBytes`: a small gzip bomb
gets `413` before it can fill memory, and corrupt input gets `400`.
""",
      exampleZIO {
        val bomb   = Compressor.gzip.compress(zio.Chunk.fill(8 * 1024 * 1024)(0.toByte))
        val routes = Routes(Method.POST / "in" -> Handler.text("reached")) @@ Middleware.decompress(maxBytes = 64.K)
        routes(Request.post("/in", Body.fromBytes(bomb)).withHeader("Content-Encoding", "gzip"))
          .map(res => (bomb.length, res.status))
      }.assert { case (compressed, status) =>
        assertTrue(compressed < 64 * 1024, status == Status.ContentTooLarge)
      },
      exampleValue {
        Brotli.encode(zio.Chunk.fromArray("hi".getBytes("UTF-8"))).nonEmpty
      }.assert(ok => assertTrue(ok)),
    ),
  )
end Http
