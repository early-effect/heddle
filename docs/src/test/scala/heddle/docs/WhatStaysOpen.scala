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
and every log the process still has. When the table is full, the next `accept`, the next
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
        assertTrue(max.toInt == 1024, backlog.toInt == 100, keep)
      },
    ),
    section("Keep-alive is a pile of sockets")(
      md"""
HTTP/1.1 leaves the connection open for the next request. That is faster until a
client goes quiet. A quiet socket still occupies a descriptor, a buffer, and a
thread.

`idleTimeout` (default 60 seconds) closes a connection that is not reading or writing.
`Duration.Infinity` is the explicit never, and it is a choice you own.

A busy connection is not closed for being busy. `maxRequestsPerConnection` defaults to
none, so a hot client is not forced to reconnect. Set `Some(KeepAliveRequests(n))` when
a balancer should see a fresh connection after n requests. That path counts. The default
does not.

A client that sends one header line every thirty seconds and never finishes the
request is not idle between requests. It is still sending. `headerTimeout`
(default 30 seconds) is that clock. Incomplete headers die even if bytes keep dribbling in.
The next section puts that clock on one shared profile.
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
    section("One profile, in ratios")(
      md"""
Heddle was compared with zio-http 3.11.6 on one shared profile.
The header cap is 16 KiB. The body cap is 10 MiB. Both are heddle's defaults,
and zio-http is configured to the same numbers, so a size row is the same cap
on both servers. One read deadline is set on both. Heddle uses it as
`headerTimeout` and as `idleTimeout`. zio-http has a single read timer, and a
byte restarts that timer. Heddle's header budget stays where it was stamped.
zio-http 3.11.6 has no connection cap. The probe sets one on heddle.

Stopwatch readings belong to the machine that ran the probe. The table is in
multiples of the shared deadline and the shared caps. `scripts/bench-safety-heddle.sh`
and `scripts/bench-safety-zio-http.sh` rerun it, one process at a time. Production
defaults stay 30 seconds, 60 seconds, and 1024 connections. The probe only shortens
the read deadline and the connection cap, so the ratios show up. Header and body
caps stay at the defaults. The relationship is the same at the default clocks.

| Probe | Against the profile | Heddle | zio-http 3.11.6 |
| --- | --- | --- | --- |
| Finished junk request line, left open or half-closed | complete request line | 400, without waiting out the deadline | 400, without waiting out the deadline |
| Partial headers, then silence | 1× the deadline | 408 | socket closes, no status |
| Partial headers, one byte every 1/5 of the deadline | the budget is the whole deadline | 408 at 1× the deadline | still open past the deadline. Each byte restarts the timer |
| Header block | 3/8 of the header cap, and again at 3/4 | 200 | 200 |
| Header block | 5/4 of the header cap | 431 | 431 |
| Body | about 1.5% of the body cap | 200 | 200 |
| Declared body, nothing sent | 11/10 of the body cap | 413 from the length | 413 from the length |
| Declared body, then the bytes | 11/10 of the body cap | 413, and the body is not read through | 413, and the body is not read through |
| `Content-Length` together with `Transfer-Encoding` | framing | 400 | 400 |
| `Content-Length` is not a run of digits | framing | 400 | 400 |
| Two `Content-Length` headers, same value | framing | 400 | 400 |
| One connection past the cap | heddle's cap is set. zio-http 3.11.6 has none | refused | served |

A body under the cap is accepted. zio-http aggregates that body. Heddle streams it.
The status is 200 either way. The body cap is the 11/10 rows: both answer 413 from
the declared length, and neither reads the body through.

Silence and a dribble are different failures. Silence ends both servers at the
deadline. Bytes that never finish the header end heddle at that same deadline.
They leave zio-http open while they keep arriving.
"""
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
`maxBodyBytes` (default 10 MiB, `BodyBytes`) is a cap on the request, not a buffer we
always allocate. `maxHeaderBytes` (default 16 KiB, `HeaderBytes`) is the header-block
cap and the per-connection read buffer: each accepted socket allocates
`max(chunkSize, maxHeaderBytes)`. HTTP/2 keeps a separate header-list cap
(`maxHeaderListSize`, default 8 KiB). `BytesLength` (`64.K`, `10.M`) is the size
type for a body or a middleware cap, not these config fields.

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
        assertTrue(headers.toInt == 16 * 1024, body.toLong == 10L * 1024 * 1024, streams.toInt == 100)
      },
    ),
    section("Gzip is a Routes wrap")(
      md"""
Compression is not a server flag. `@@` wraps **this** `Routes` value. `++` tries
the left, then the right when the left has no matching route. A response the left
produced stands, including a 404 or a 405.

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

Interrupting a call (a `timeout`, a lost race) closes its socket and frees its slot. A body
that ends when the server closes the socket (no `Content-Length`, not chunked) is never pooled,
and neither is a `Connection: close` exchange or an HTTP/1.0 one without keep-alive. Bodies
past `maxBodyBytes` fail with `ClientError.Protocol(_, HttpError.BodyTooLarge)`, however they
are framed.

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
        assertTrue(n.toInt == 10, connect == 10.seconds, poolIdle == 60.seconds)
      },
    ),
  )
end WhatStaysOpen
