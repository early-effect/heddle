# Heddle

HTTP as an effect. Loom under the floor.

A ZIO-first HTTP library: handlers are `Request => ZIO[R, E, Response]`, routing is data, middleware is `@@`, and OpenAPI 3.1 / Swagger UI are generated from endpoints. User JSON still goes through a `JsonCodec` (add `heddle-zio-json`, or bring circe/jsoniter). Core uses zio-json only for library-owned documents (the OpenAPI spec).

`import heddle.*` is a curated facade over `heddle.http`, `heddle.route`, `heddle.endpoint`, `heddle.server`, `heddle.client`, and `heddle.error`. SSE, WebSocket, and Datastar stay in their own packages and are not on the facade.

Methods are RFC 9110 plus PATCH; anything else is `Method.Custom` (CONNECT is named). Status is the RFC 9110 catalog plus 103, 425, 428, 429, 431, 451, and 506-511; 413 is `ContentTooLarge`; unknown codes are `Status.fromCode` with text `"Unknown"`. Header names are interned `HeaderName` values. `TypedHeader` parses on read (`headers.get[MediaType]`, cookies, `Authorization`, `Host`, dates).

`Body.json` / `Body.text` use `MediaType.JsonUtf8` / `TextUtf8`. OpenAPI content keys are charset-less (`application/json`).

## Install

Core (ZIO only):

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle" % "VERSION"
```

Optional zio-json adapter:

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle-zio-json" % "VERSION"
```

Optional brotli encoder (our code, RFC 7932, no JNI):

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle-brotli" % "VERSION"
```

## Routes

```scala
import heddle.*
import zio.*

val routes = Routes(
  Method.GET / "health"            -> Handler.text("ok"),
  Method.GET / "users" / int("id") -> { (id: Int) =>
    ZIO.succeed(Response.text(id.toString))
  },
)

val app = routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug)

object Hello extends HeddleApp:
  def routes = app
```

Bodies on the wire are streams. `Body.asString` / `asBytes` are for in-memory bodies; use `collect` otherwise. `maxBodyBytes` is a cap, not a buffer.

Coming from zio-http 3: `Routes`, `handler`, `@@`, and `Server.serve` are the same shape. `Response.fromServerSentEvents` is `Sse.response` / `Sse.session`. JSON is an optional `JsonCodec` (`heddle-zio-json`). There is no Netty and no event-loop rule; accept/read/write are ordinary ZIO on Loom.

A caller-given file is `Files.fromPath`. Compression is `@@ Middleware.compress` (gzip in core; add `heddle-brotli` and pass `Brotli.compressor` when you want `br`). Incoming `Content-Encoding` is `@@ Middleware.decompress` (`Decompressor.gzip` in core; pass `Brotli.decompressor` for `br`). TLS is a compose-time layer, not a `Protocol` flag:

```scala
Server.serve(app).provide(Server.Config.defaults, Tls.pem(certPem, keyPem))
```

ALPN offers `h2` and `http/1.1` on that bind. Cleartext still speaks both: the 24-byte HTTP/2 preface is h2c prior-knowledge, anything else is HTTP/1.1.

## Server-Sent Events

```scala
import heddle.*
import heddle.sse.*
import zio.*
import zio.stream.ZStream

val ticks = Routes(
  Method.GET / "ticks" -> handler(
    ZIO.succeed(
      Sse.response(
        ZStream.tick(1.second).zipWithIndex.map { (_, n) =>
          ServerSentEvent(data = n.toString, event = Some("tick"), id = Some(n.toString))
        }
      )
    )
  ),
  Method.GET / "session" -> handler { (_: Request) =>
    Sse.session { w =>
      w.send(ServerSentEvent("hello")) *> w.heartbeat
    }
  },
)
```

Each event is flushed as its own HTTP chunk. JSON in `data` is `JsonCodec.encode`; core does not couple SSE to JSON.

## Endpoints and OpenAPI

```scala
import heddle.*
import heddle.json.given
import zio.json.{JsonDecoder, JsonEncoder}

final case class User(id: Int, name: String) derives Schema, JsonEncoder, JsonDecoder
final case class NotFound(message: String) derives Schema, JsonEncoder, JsonDecoder

val getUser =
  Endpoint
    .get("users" / int("id"))
    .out[User]
    .outError[NotFound](Status.NotFound)
    .summary("Get a user")
    .tag("users")

val spec = OpenApi.from("Users", "0.1.0", getUser)

val app =
  getUser.implement(id => findUser(id)) ++
    spec.routes("docs") // GET /docs and GET /docs/openapi.json
```

`.inJson` / `.out` / `.outError` need a `JsonCodec` in scope. `import heddle.json.given` after adding `heddle-zio-json` is enough for any type with zio-json instances. Other libraries (circe, jsoniter) satisfy the same type class:

```scala
given [A](using enc: io.circe.Encoder[A], dec: io.circe.Decoder[A]): JsonCodec[A] =
  JsonCodec.from(
    a => enc(a).noSpaces,
    s => dec.decodeJson(io.circe.parser.parse(s).getOrElse(io.circe.Json.Null)).left.map(_.message),
  )
```

User bodies go through `JsonCodec`; core does not pick a codec for your types. OpenAPI is library-owned JSON: core encodes the spec ADT with zio-json so Swagger does not need a user `JsonCodec`.

`Server.install` / `Server.serve` shift onto ZIO's Loom executor (`Runtime.enableLoomBasedExecutor`). Accept and connections are ordinary ZIO fibers; they need JDK 21+.

## Server config

`Server.Config` carries bind address, HTTP/1.1 limits, NIO `chunkSize` (8 KiB), and `gracefulShutdownTimeout` (10s). Incoming bodies are streams (never buffered whole); `maxBodyBytes` (10 MiB) is a cap, not a buffer. Defaults:

```scala
Server.serve(app).provide(Server.Config.defaults)

Server.serve(app, Server.Config.default.copy(port = 8080, maxBodyBytes = 1_000_000))

// heddle.server.host, heddle.server.port, heddle.server.maxHeaderBytes, ...
Server.serve(app).provide(Server.Config.layer)
```

## Modules

| Artifact | Depends on | Role |
| --- | --- | --- |
| `heddle` | ZIO | HTTP types, routes, middleware, Loom server, Schema, Endpoint, OpenAPI |
| `heddle-zio-json` | heddle, zio-json | `given JsonCodec[A]` from zio-json |
| `heddle-brotli` | heddle | RFC 7932 `br` encoder / decoder (`Brotli.compressor`, `Brotli.decompressor`) |

## Stress and leaks

Mission-critical leak smoke lives in `heddle` (`LeakSmokeSpec`) and runs with `sbt heddle/testFull`.

Slow soak / abort / stair-step proofs are **not** in the root aggregate:

```bash
sbt perfTests/testFull
```

Heddle vs zio-http 3 plaintext (local, needs bombardier or wrk):

```bash
./scripts/bench-compare.sh
```

JFR of a 30s load:

```bash
./scripts/profile-heddle.sh   # writes target/profile/heddle.jfr
```

## Run the example

```bash
sbt example/run
```

Then open http://localhost:8080/docs (users API), http://localhost:8080/preview (static files + SSE reload), or http://localhost:8080/session (`Sse.session`).
