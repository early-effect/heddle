# SSE and HTTP/2 so heddle can replace zio-http

Status: in progress. Slice 1 (SSE on HTTP/1.1) is the first cut.

Goal: heddle grows SSE, files, compression, WebSockets, and HTTP/2. Then `publishLocal`, and open branches on **ascent** and **specular** (specular depends on ascent) that delete zio-http entirely. After heddle is released, those branches adopt the Central artifact with a version bump.

## Why this, not the old plan

That doc was written before the engine existed. Several of its premises are now false:

| Old plan | Now |
|---|---|
| `Body.Stream` is the prerequisite | Already in core. HTTP/1.1 unknown-length writes are chunked (`runForeachChunk` + `chunkedFrame`). `zio-streams` is already a core dep. |
| Engine may `unsafe.run` on a stream VT, "same as HTTP/1.1 today" | HTTP/1.1 never does that. Handlers are ZIO on the Loom runtime. H2 must fork **ZIO fibers** per stream, not `Runtime.unsafe.run`. |
| Copy zio-http v4 as the product | v4 Loom is h2c-only, effect-agnostic, ZIO bolted on via `Unsafe.run`. Ignore it as a target. Steal **HPACK ordering and reader/writer split** only. |
| Streaming `Body` is the first slice | First real gap is **SSE framing + per-event flush**, then static files, then H2. |

zio-http 3 (Netty) is still the thing we replace in ascent/specular. Local numbers: heddle HTTP/1.1 plaintext ~94% of zio-http 3. That path stays.

## What the products actually need

**ascent-preview** (`~/projects/fun/ascent/preview`): path-jailed static files + `GET /__ascent/reload` as SSE. Stamp-file poll, emit `ServerSentEvent("reload", Some("reload"))`. `Preview.serve` is `Server.install` + `ZIO.never` in the caller's Scope, extra routes in front of trailing GET, optional CORS. Specular does not run its own server; `specularPreview` **is** `ascentPreview`.

**ascent-datastar-http**: thin wrapper over `zio-http-datastar-sdk`. `events { handler }` puts `Datastar` in `R`, handler offers frames to a queue, response is `text/event-stream`. `AscentDatastar.patchRegion` / `patchSignal` call `ServerSentEventGenerator`. Counter and hybrid-chat compose those API routes with `Preview.serve`. Browser side is `EventSource` (HTTP/1.1 today).

**specular site**: `zio.http.Client` for GitHub `metadata.json` only. Heddle `Client` already exists (HTTP/1.1, collects Content-Length). Switch later; not a server feature.

Heddle does **not** grow a Datastar SDK. SSE primitives + a queue-backed session are enough for ascent to rewrite `AscentDatastar` against `heddle.sse`.

Local preview is cleartext HTTP (`:8765` / `:8080`). Browsers speak HTTP/2 only after TLS + ALPN. Implementation order is still SSE on HTTP/1.1 first (what preview already uses), then the H2 engine on the same bind (cleartext preface + later ALPN). The running server does not make the app pick one protocol.

The handler API is already complete (`Request => ZIO[R, E, Response]`, `@@`, ZStream bodies). Nothing here asks to change it. WebSockets are a new handler that still returns a `Response` (the upgrade) and then runs a ZIO session on the same connection.

## Non-goals (this work)

- Netty, `Unsafe`, JNI (brotli is our code, not brotli4j).
- A Datastar module in heddle (SSE session is the primitive; ascent keeps the protocol).
- HTTP/1.1 `Upgrade: h2c` (prior-knowledge + ALPN cover tests and browsers).
- `Last-Event-ID` resume.

## Safety (load-bearing, from the HTTP/1.1 work)

- Blocking I/O only via `ZIO.attempt` on `Runtime.enableLoomBasedExecutor`.
- HTTP/1.1: per-connection sequential read/write (Swagger hang was concurrent R/W on one channel).
- HTTP/2: one reader fiber + one writer fiber/queue per connection. Streams are concurrent ZIO fibers. Frames never interleave; HPACK encode+write under one connection lock (RFC 7541 §2.3.2). Flow control is ZIO (wait on window), not `Condition.park` on a raw VT.
- Chunks given to user code are owned. No "invalid after next read".
- Long-lived SSE is in-flight work: `busy` stays true until the stream ends or the client drops. Halt still: stop accept, close idle, grace in-flight (including SSE), force.
- Client disconnect must interrupt the ZStream (write failure / channel close).
- Flush **per SSE event**. Do not accumulate to `chunkSize` / `maxFrameSize`. Inter-event delay is the payload contract.
- Never Content-Encoding-compress `text/event-stream` or a WebSocket upgrade.

---

## Slice 1: SSE on HTTP/1.1

Unblocks ascent-preview and datastar examples without H2.

```scala
package heddle.sse

final case class ServerSentEvent(
  data: String,
  event: Option[String] = None,
  id: Option[String] = None,
  retry: Option[Duration] = None,
)

object SseCodec:
  val heartbeat: Chunk[Byte]  // ": ping\n\n"
  def encode(event: ServerSentEvent): Chunk[Byte]
  def decode(bytes: Chunk[Byte]): (Chunk[ServerSentEvent], Chunk[Byte]) // leftover for tests/client

object Sse:
  def body[R](events: ZStream[R, Throwable, ServerSentEvent]): Body
  def response[R](events: ZStream[R, Throwable, ServerSentEvent]): Response
  def heartbeatEvery(d: Duration): ZStream[Any, Nothing, ServerSentEvent]
  /** Queue-backed session: handler writes events, response is the SSE stream. Ascent `events { }` shape. */
  def session[R](use: Sse.Writer => ZIO[R, Throwable, Unit]): ZIO[R & Scope, Nothing, Response]
```

- Construction rejects CR/LF in `event` / `id`. `data` may be multiline (`data:` per line).
- `Sse.response`: `200`, `Content-Type: text/event-stream`, `Cache-Control: no-cache`, `Connection: keep-alive` optional, unknown-length `Body.Stream` (one ZStream chunk = one encoded event).
- HTTP/1.1 writer already chunked-frames each `runForeachChunk`. Keep that. Prove with a paced e2e that inter-arrival stays near `ZStream.tick`, not coalesced to 8KiB.
- `Sse.session`: unbounded queue, fork `use(writer)`, stream until writer completes or scope closes. This is the datastar `events { }` pattern without Datastar types.
- `Endpoint.outSse` documents `text/event-stream` in OpenAPI. JSON in `data` is the user's `JsonCodec.encode`. No SSE/JSON coupling in core.
- Client: incremental `text/event-stream` parse (for tests). Today's Client collecting Content-Length cannot drive SSE e2e.

Tests:

- `SseCodec`: multiline data, heartbeat comment, reject CR in `id`.
- Chunked H1 unknown-length already exists; add: SSE headers, one HTTP chunk per event.
- Paced `ZStream.tick` e2e against `LiveServer`: Client reads event-by-event, gaps in band, bytes = `SseCodec.encode`.
- Interrupt: client close while streaming; handler fiber is interrupted; no hang on `Server.shutdown`.
- `Sse.session`: two writes, complete, client sees both then end.

Files: new `heddle/sse/ServerSentEvent.scala`, `SseCodec.scala`, `Sse.scala`; `Http1.scala` only if flush is wrong; `Client.scala` streaming read; `Endpoint.scala` `outSse`; `Http1Spec` / new `SseSpec`.

## Slice 2: Static files

ascent-preview is `Handler.fromFile` + trailing GET + path jail. Heddle has CORS and `Server.install` (returns `Server` with `port`). Missing: file body.

```scala
object Files:
  def fromPath(path: Path): Task[Response]  // 200 + Body.Stream from FileChannel, Content-Type from extension, Content-Length
```

Known-length `Body.Stream` already writes `Content-Length` + pull. Path jail stays in ascent-preview (canonical `startsWith`). Heddle just streams a file it is given.

Tests: small file equals bytes; directory / missing → caller 404 (or `Files` returns a failed IO); `Content-Type` for `.html` / `.js` / `.css`.

This plus SSE is enough for ascent-preview on HTTP/1.1.

## Slice 3: Compression middleware + brotli module

Not a `Server.Config` flag. You may compress the static/docs routes and leave `/sse` alone:

```scala
(api ++ files) @@ Middleware.compress() ++ Sse.routes
```

```scala
enum ContentEncoding:
  case Gzip, Brotli, Identity
  def token: String = this match
    case Gzip     => "gzip"
    case Brotli   => "br"
    case Identity => "identity"

trait Compressor:
  def encoding: ContentEncoding
  def compress(bytes: Chunk[Byte]): Chunk[Byte]
  def stream(in: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte]

object Middleware:
  def compress(
      minBytes: Int = 1024,
      compressors: Chunk[Compressor] = Chunk(Compressor.gzip),
  ): Middleware[Any]
```

`Accept-Encoding` parsing yields `Chunk[ContentEncoding]`, not strings. `Content-Encoding` on the response is `encoding.token` only at the wire.

- `@@` as today (`Routes => Routes`). Negotiate `Accept-Encoding` per request; pick the first compressor the client listed (q-values later if we need them). Prefer listing brotli before gzip when both are on the app.
- Skip: `text/event-stream`, WebSocket upgrades, `image/*` / `video/*` / `zip`, already `Content-Encoding`. Known-length bodies lose `Content-Length` and go chunked (or H2 DATA).
- Stream with sync flush. Do not collect a file just to compress it.
- **gzip** `given` in core (`java.util.zip`).
- **brotli** is a published `heddle-brotli` module we **write**. RFC 7932. Reference implementations (Google brotli C, existing Java ports) are reading material, not a JNI dep. Encoder good enough for HTTP (quality ~4–6 streaming, higher for static). Round-trip tests against a known decoder. Core never depends on the module; `Middleware.compress(compressors = Chunk(Brotli.compressor, Compressor.gzip))` is an app `given` once `heddle-brotli` is on the classpath.

Tests: gzip JSON; brotli JSON (in the module); skip SSE even under `@@ compress`; `identity` uncompressed; compress only the `@@` subset of a `++` tree.

## Slice 3b: TLS as composition, not Protocol config

Same idea as compression: you apply it where you want it. A connection is TLS or it is not (handshake is before `Request`), so TLS is **not** a `Routes => Routes` that encrypts some paths on a cleartext socket. It is a bind you compose at serve time:

```scala
Server.serve(routes @@ Middleware.compress()).provide(Server.Config.defaults)
Server.serve(routes @@ Middleware.compress()).provide(Server.Config.defaults, Tls.layer(cert, key))
```

HTTP/1.1 and H2 both work cleartext or TLS. ALPN (`h2`, `http/1.1`) lives on the TLS layer, not on `Protocol.H2(tls)`. `Middleware.requireTls` (301/403 if the request was not on a TLS bind) is the per-route piece, for apps that mix a cleartext preview port with HTTPS API.

Slice 7 is this layer + ALPN, not a `Protocol` constructor that smuggles certs.

## Slice 4: WebSockets (HTTP/1.1)

RFC 6455. This is a first-class transport, not a follow-up. Same halt/interrupt story as SSE: the session is in-flight until close or client drop.

Handshake: `Upgrade: websocket`, `Connection: Upgrade`, `Sec-WebSocket-Key` / `Accept` (SHA-1 as the RFC requires). After 101, that connection is no longer HTTP/1.1 keep-alive.

```scala
enum WebSocketFrame:
  case Text(text: String)
  case Binary(bytes: Chunk[Byte])
  case Ping(bytes: Chunk[Byte])
  case Pong(bytes: Chunk[Byte])
  case Close(code: Int, reason: String)

trait WebSocket:
  def receive: ZStream[Any, Throwable, WebSocketFrame]
  def send(frame: WebSocketFrame): Task[Unit]
  def close(code: Int = 1000, reason: String = ""): Task[Unit]

object Handler:
  def websocket[R](run: WebSocket => ZIO[R, Throwable, Unit]): Handler[R, Nothing]
```

- Frame codec (FIN, opcodes, masking on client frames, size limits).
- Auto-answer Ping with Pong unless the session takes pings.
- `run` is ordinary ZIO. Interrupt (halt, client close, fiber interrupt) sends Close if possible, then drops the socket.
- Per-message deflate (RFC 7692) is not in the first WS PR; can follow once compression exists.

HTTP/2 extended CONNECT (RFC 8441) waits until the H2 engine exists (slice 6). Browsers on cleartext preview still use HTTP/1.1 Upgrade, which is this slice.

Tests: handshake 101; echo text; ping/pong; client close interrupts `run`; server `close` is a Close frame; oversized frame is 1009; two sequential WS upgrades on different connections (not pipelined on one).

## Slice 5: HTTP/2 codec (no sockets)

Stay in the `heddle` artifact (`heddle.h2`). No extra published module until the codec is large enough to justify a split.

```
heddle.h2/
  H2Frame, FrameCodec
  hpack/{Hpack, StaticTable, DynamicTable, Huffman}
  H2Settings
```

RFC 7540 frames, RFC 7541 HPACK. SETTINGS validation helpers (`maxFrameSize` in 16KiB–16MiB-1). Tests from the RFC examples. Independent of the HTTP/1.1 server.

Do not take v4's `@experimental` infection. This is ordinary Scala 3.

## Slice 6: HTTP/2 engine, same bind as HTTP/1.1

The app does not pick a protocol. The **client** does. Default `Server.serve` speaks HTTP/1.1 and HTTP/2 on one port. `Http2Config` is knobs (windows, max streams), not an exclusive mode. An opt-out `http2 = false` exists for a tiny embed; it is not the default.

How this actually works on the wire:

| Bind | How the client says which HTTP | Who uses it |
|---|---|---|
| Cleartext (preview `:8765`) | First bytes: `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` → HTTP/2 **prior knowledge** (h2c). Anything else → HTTP/1.1. | curl `--http2-prior-knowledge`, `h2load`, gRPC-c, tests. **Browsers do not.** They will not do h2c on `http://`. |
| TLS (`Tls.layer`) | ALPN during the handshake: client offers `h2` and/or `http/1.1`, server picks. No preface guess. | Browsers, `curl --http2`, Java `HttpClient` HTTP/2. This is ordinary HTTPS. |

There is no third “H2-only” default. v4’s h2c-only server (RST on HTTP/1.1) is the thing we are not repeating.

We skip HTTP/1.1 `Upgrade: h2c` (the old cleartext upgrade dance). Prior knowledge + ALPN cover tests and browsers.

**Connection (HTTP/2 path):**

1. After preface or ALPN `h2`: write SETTINGS, read peer SETTINGS, ACK.
2. Reader fiber: frames → HPACK decode **in wire order** on this fiber → open/reset streams.
3. New stream: **fork a ZIO fiber** that runs `routes(request)` (same dispatch as H1). `:method` `:path` `:scheme` `:authority` → `Request`. Response `:status` + headers via HPACK.
4. Writer fiber: one queue of frames; HEADERS encode+write under the connection lock so encode order = wire order.
5. Flow control: DATA respects connection and stream windows. If the window is 0, the stream fiber waits in ZIO, it does not `park` a raw VT.
6. HEAD: headers only, `END_STREAM` on HEADERS.
7. Known-length body: DATA split at `maxFrameSize`. Unknown-length (SSE): DATA per event, then empty DATA + `END_STREAM` when the ZStream completes.
8. `maxBodyBytes` counted incrementally on DATA. Oversize → `RST_STREAM`. Declared content-length above the cap rejected before body bytes.
9. Timeouts as ZIO. `Http2Config`: `maxConcurrentStreams` (100), `initialWindowSize` (65535), `maxFrameSize` (16384), `maxHeaderListSize`.

Cleartext detect: read enough bytes to see the 24-byte preface vs `GET ` / `POST ` / … then hand the leftover into `Http1` or `H2Connection`. Same `accept` loop as today.

Halt: same three phases. H2 GOAWAY then wait in-flight streams, then force close.

Tests:

- Same port: bombardier HTTP/1.1 still works; `h2load` prior-knowledge works.
- GET/POST, 404/405, two concurrent streams on one connection.
- Body cap → RST. SETTINGS / window.
- SSE over H2: same pacing as slice 1.
- Shutdown during an open SSE stream.

Files: `H2Connection`, preface detect in `Server.scala`, `FlowController`, `Http2Config` on `Server.Config`.

**WebSockets on H2:** RFC 8441 extended CONNECT (`:method CONNECT`, `:protocol websocket`). Same `Handler.websocket` API. HTTP/1.1 Upgrade stays for cleartext.

## Slice 7: TLS layer + ALPN

`Tls.layer(cert, key)` (or `Tls.pem(...)`) wraps the accepted socket with JDK `SSLEngine` / `SSLSocket`. ALPN offers `h2` and `http/1.1`; the client’s list decides. Same routes, same SSE / WS / compress middleware.

E2e: Java `HttpClient` HTTP/2 (ALPN `h2`). Curl `--http2`. Curl default HTTP/1.1 over TLS. Cert fixture. One HTTPS port, both protocols.

Preview stays cleartext (H1 + h2c prior-knowledge). Production-shaped servers provide the TLS layer and browsers get `h2` for free.

## Slice 8: publishLocal and adoption branches

Heddle `example/` still gets a miniature preview (static + SSE reload) and an `Sse.session` datastar-shaped route so the library is self-proving. README maps `fromServerSentEvents` → `Sse.response` / `Sse.session`.

Then prove the products, not a toy:

1. `sbt --no-server publishLocal` in heddle (`heddle`, `heddle-zio-json`, `heddle-brotli`).
2. **ascent branch** (depends on that local version in `ZipxVersions`):
   - `ascent-preview` onto heddle: `Sse.response`, `Files.fromPath`, `Middleware.cors`, `Server.install` / `HeddleApp`.
   - `ascent-datastar-http` onto `Sse.session` + datastar event encoding (reimplement `ServerSentEventGenerator` against heddle; drop `zio-http-datastar-sdk`).
   - Counter and hybrid-chat: `(api ++ files) @@ Middleware.compress(compressors = Chunk(Brotli.compressor, Compressor.gzip))` (depends on `heddle-brotli`). SSE routes stay outside that `@@`.
   - Docs `ServeSite` / datastar doc pages.
   - Remove `zioHttp` and `zioHttpDatastarSdk` from `ZipxVersions`. No leftover `import zio.http`.
   - `publishLocal` the ascent artifacts the specular branch needs (`ascent-preview`, `ascent-html`, …).
3. **specular branch** (on the ascent branch / local ascent + local heddle):
   - `specularPreview` stays `ascentPreview`; it picks up heddle transitively.
   - `ProjectMetaHttp` / `SiteModel` onto heddle `Client`.
   - Remove `zioHttp` from `ZipxVersions`. No leftover `import zio.http`.

Keep those branches green (`testFull`, preview compile) against `publishLocal`. Do not merge them onto ascent/specular main until heddle is on Central; the first commit after release is a version bump from local to the published coordinate. Humans merge.

---

## PR stack

1. **SSE on HTTP/1.1** (codec, `Sse.response` / `session`, paced e2e, client incremental read, `outSse`). Rewrite `docs/plans/sse-http2.md` from the parked file in the same PR or immediately before.
2. **Static files** (`Files.fromPath`).
3. **Compression middleware** (gzip in core) **and `heddle-brotli`** (our encoder).
4. **WebSockets on HTTP/1.1** (upgrade, frames, `Handler.websocket`).
5. **H2 codec + HPACK** (no sockets).
6. **HTTP/2 engine** on the same bind (preface detect + H1 leftover). SSE-on-H2 e2e. WS over H2 (RFC 8441) in this PR or immediately after.
7. **TLS layer + ALPN** (`h2` and `http/1.1` from what the client offers).
8. **Examples + README** in heddle, then `publishLocal`.
9. **ascent adoption branch** (no zio-http).
10. **specular adoption branch** (no zio-http; after ascent `publishLocal`).

PRs 1–2 unblock ascent-preview on HTTP/1.1. PR 3 is compression middleware + brotli. PR 4 is WebSockets. PR 6 is dual-stack cleartext (H1 + h2c). PR 7 is dual-stack HTTPS (ALPN). PRs 9–10 are the product proof; they sit until heddle is released.

## Config additions (H2c slice)

Keep existing `host` / `port` / `maxHeaderBytes` / `maxBodyBytes` / `chunkSize` / `gracefulShutdownTimeout`. Add `Http2Config` and `http2: Boolean = true`. No exclusive `Protocol` enum. TLS is a `ZLayer`. Compression is `@@ Middleware.compress`. `maxBodyBytes` applies to H1 and H2. Reject illegal `maxFrameSize` at construction.

## Success

- `sbt --no-server "heddle/testFull; json/testFull"` and `example/compile`.
- SSE H1 e2e: paced events, interrupt, shutdown.
- gzip and brotli e2e: JSON/file compressed via `@@ Middleware.compress`; SSE uncompressed.
- WebSocket echo + interrupt + Close.
- Dual-stack e2e: HTTP/1.1 bombardier and h2c `h2load` on the same port; concurrent streams; SSE flush; body cap RST.
- HTTPS e2e: Java HttpClient ALPN `h2` and HTTP/1.1 on the same TLS port.
- A ZIO engineer can write `Sse.response(ZStream.tick(...).map(...))`, `Sse.session(w => w.send(event))`, and `Handler.websocket(ws => ...)` without learning an engine.
- `publishLocal` heddle; ascent and specular branches compile and test with **zero** `zio-http` / `zio-http-datastar-sdk` dependencies.
