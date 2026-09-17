package heddle.docs

import heddle.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object WhatStaysOpen extends DocSpecSuite:

  def doc = page("What stays open")(
    md"""
A running server is a process that holds files. Every accepted TCP client spends a file
descriptor. So does every idle socket in a client pool, every open file you are serving,
and every log the JVM still has. When the table is full, the next `accept`, the next
outbound call, and the next log line all fail the same way.

This page is the clocks and caps that keep that table from growing without bound.
The [HTTP](http.html) page is the surface. [Reference](reference.html) is the full field list.
""",
    section("A connection is a file")(
      md"""
`maxConnections` is the cap we put on ourselves before the operating system does.
The default is 1024. The extra handshake is closed, not queued forever.

The listen backlog is a different number. `soBacklog` (default 100) is how many
handshakes the kernel will hold before it starts refusing. Independent of how many
connections we are already serving.
""",
      exampleValue {
        (
          Server.Config.default.maxConnections,
          Server.Config.default.soBacklog,
          Server.Config.default.soKeepAlive,
        )
      }.assert { case (max, backlog, keep) =>
        assertTrue(max == 1024, backlog == 100, keep)
      },
    ),
    section("Keep-alive is a pile of sockets")(
      md"""
HTTP/1.1 leaves the connection open for the next request. That is faster until a
client goes quiet. A quiet socket still occupies a descriptor, a buffer, and a
thread.

`idleTimeout` (default 60 seconds) closes a connection that is not reading or writing.
`Duration.Infinity` is the explicit never, and it is a choice you own.

A client that sends one header line every thirty seconds and never finishes the
request is not idle between requests. It is still sending. `headerTimeout`
(default 30 seconds) is that clock. Incomplete headers die even if bytes keep dribbling in.
""",
      exampleValue {
        (
          Server.Config.default.idleTimeout,
          Server.Config.default.headerTimeout,
          Server.Config.default.copy(idleTimeout = Duration.Infinity).idleTimeout,
        )
      }.assert { case (idle, header, never) =>
        assertTrue(idle == 60.seconds, header == 30.seconds, never == Duration.Infinity)
      },
    ),
    section("The handler timeout is not the socket timeout")(
      md"""
`Middleware.timeout` turns a slow route into 504. It does not close the TCP
connection by itself. Socket clocks live on `Server.Config`. Put the 504 on the
`Routes` that should have it; put idle and header limits on the bind.
""",
      exampleValue {
        Middleware.timeout(2.seconds)
        true
      }.assert(ok => assertTrue(ok)),
    ),
    section("Uploads and memory")(
      md"""
`maxBodyBytes` (default `10.M`) is a cap on the request, not a buffer we always
allocate. `maxHeaderBytes` is `64.K`. `64.K` and `10.M` are `BytesLength`: a size,
not a collection of bytes.

If the handler never reads the body, the server still drains leftovers so the next
keep-alive request is not garbage.

HTTP/2 windows exist so a fast sender cannot fill an unbounded queue while the
handler is slow. DATA past the stream window is reset. `maxConcurrentStreams`
(default 100) refuses extras. Writer and body queues are bounded
(`http2Config.maxOutstandingFrames`, default 64).
""",
      exampleValue {
        (
          Server.Config.default.maxHeaderBytes,
          Server.Config.default.maxBodyBytes,
          Server.Config.default.http2Config.maxConcurrentStreams,
        )
      }.assert { case (headers, body, streams) =>
        assertTrue(headers == 64.K, body == 10.M, streams == 100)
      },
    ),
    section("Gzip is a Routes wrap")(
      md"""
Compression is not a server flag. `@@` wraps **this** `Routes` value. `++` tries
the left, then the right on 404/405.

Global gzip is still one line, at composition time:

```scala
val app = (api ++ files) @@ Middleware.compress() ++ sse
Server.serve(app).provide(Server.Config.defaults)
```

Partial gzip leaves event streams alone:

```scala
val app = (api @@ Middleware.compress()) ++ files ++ sse
```

If gzip lived on `Server.Config`, you would compress everything on the bind, including
routes that must not be content-encoded, and you would have no way to say "these yes,
those no" without a second process.

Incoming `Content-Encoding` is `@@ Middleware.decompress(maxBytes = ...)`. Inflating
without a cap is how a small request becomes a large heap. The default cap is
`Server.Config.defaultMaxBodyBytes`.
""",
      exampleZIO {
        val compressed =
          Routes(Method.GET / "a" -> Handler.text("a" * 2048)) @@ Middleware.compress(minBytes = 16)
        val plain  = Routes(Method.GET / "b" -> Handler.text("b" * 2048))
        val routes = compressed ++ plain
        for
          a <- routes(Request.get("/a").withHeader("Accept-Encoding", "gzip"))
          b <- routes(Request.get("/b").withHeader("Accept-Encoding", "gzip"))
        yield (
          a.header("Content-Encoding").contains("gzip"),
          b.header("Content-Encoding").isEmpty,
        )
      }.assert { case (gz, raw) =>
        assertTrue(gz, raw)
      },
    ),
    section("Files and upgrades")(
      md"""
Serving a file opens a channel that must close on interrupt. A WebSocket or SSE
that goes quiet is still a connection. It uses the same idle story unless you set
`Duration.Infinity` because the protocol is long-lived on purpose.
""",
      exampleValue {
        Server.Config.default.gracefulShutdownTimeout
      }.assert(g => assertTrue(g == 10.seconds)),
    ),
    section("The client is a process too")(
      md"""
A pool of idle sockets to a host you stopped calling still counts as open files.
`Client.Config` caps in-flight connections per host (`maxConnectionsPerHost`, default 10)
and idle keepers (`maxIdlePerHost`, default 10). `poolIdleTimeout` (default 60 seconds)
closes idle pooled sockets. `connectTimeout` (default 10 seconds) and `idleTimeout`
(default 60 seconds) stop a hung peer from blocking a fiber forever.

`Client.get` does not add `Accept-Encoding`. If you want gzip, set the header.
The client inflates a gzip body only when that request asked for gzip.
""",
      exampleValue {
        (
          Client.Config.default.maxConnectionsPerHost,
          Client.Config.default.connectTimeout,
          Client.Config.default.poolIdleTimeout,
        )
      }.assert { case (n, connect, poolIdle) =>
        assertTrue(n == 10, connect == 10.seconds, poolIdle == 60.seconds)
      },
    ),
  )
end WhatStaysOpen
