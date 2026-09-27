package heddle.route

import heddle.auth.Auth
import heddle.endpoint.ApiKeyIn
import heddle.error.HttpError
import heddle.BytesLength
import heddle.http.{Body, ContentEncoding, Method, Request, Response, Status}
import heddle.http.header.{BasicCredentials, HeaderName}
import heddle.server.{Compressor, Decompressor, Files, SafePath}
import heddle.Server
import zio.*
import zio.Chunk

trait Middleware[-R]:
  def apply[R1 <: R, E](routes: Routes[R1, E]): Routes[R1, E]

  def ++[R1 <: R](that: Middleware[R1]): Middleware[R1] =
    val self = this
    new Middleware[R1]:
      def apply[R2 <: R1, E](routes: Routes[R2, E]): Routes[R2, E] =
        that(self(routes))

object Middleware:
  val identity: Middleware[Any] =
    new Middleware[Any]:
      def apply[R1 <: Any, E](routes: Routes[R1, E]): Routes[R1, E] = routes

  def basicAuth(username: String, password: String): Middleware[Any] =
    basicAuth((c: BasicCredentials) => c.username == username && c.password == password)

  def basicAuth(validate: BasicCredentials => Boolean): Middleware[Any] =
    interceptZIO(req => Auth.basic(validate)(req).as(req))

  def basicAuthZIO[R](validate: BasicCredentials => ZIO[R, Nothing, Boolean]): Middleware[R] =
    interceptZIO { req =>
      Auth
        .basicZIO[R] { c =>
          validate(c).flatMap(ok => if ok then ZIO.succeed(c) else ZIO.fail(Auth.unauthorizedBasic))
        }(req)
        .as(req)
    }

  def bearerAuth(validate: String => Boolean): Middleware[Any] =
    interceptZIO { req =>
      Auth.bearer(t => if validate(t) then ZIO.succeed(t) else ZIO.fail(Auth.unauthorizedBearer))(req).as(req)
    }

  def bearerAuthZIO[R](validate: String => ZIO[R, Nothing, Boolean]): Middleware[R] =
    interceptZIO { req =>
      Auth
        .bearer { t =>
          validate(t).flatMap(ok => if ok then ZIO.succeed(t) else ZIO.fail(Auth.unauthorizedBearer))
        }(req)
        .as(req)
    }

  def apiKey(header: String, validate: String => Boolean): Middleware[Any] =
    interceptZIO { req =>
      Auth
        .apiKey(
          ApiKeyIn.Header,
          header,
          v => if validate(v) then ZIO.succeed(v) else ZIO.fail(Auth.unauthorizedApiKey(header)),
        )(req)
        .as(req)
    }

  def intercept(f: Request => Request): Middleware[Any] =
    interceptZIO(req => ZIO.succeed(f(req)))

  def interceptZIO[R](f: Request => ZIO[R, Response, Request]): Middleware[R] =
    wrap[R] { [R1 <: R, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        f(req).foldZIO(res => ZIO.succeed(res), handler.run)
      }
    }

  def mapResponse(f: Response => Response): Middleware[Any] =
    mapResponseZIO(res => ZIO.succeed(f(res)))

  def mapResponseZIO[R](f: Response => ZIO[R, Nothing, Response]): Middleware[R] =
    wrap[R] { [R1 <: R, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        handler.run(req).flatMap(f)
      }
    }

  def debug: Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        Clock.nanoTime.flatMap { start =>
          handler.run(req).tap { res =>
            Clock.nanoTime.flatMap { end =>
              val ms = (end - start) / 1_000_000
              ZIO.logInfo(s"${req.method.render} ${req.url.render} -> ${res.status.code} ${ms}ms")
            }
          }
        }
      }
    }

  def timeout(duration: Duration): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        handler.run(req).timeout(duration).map {
          case Some(res) => res
          case None      => Response.empty(Status.GatewayTimeout)
        }
      }
    }

  def requestLog: Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        Clock.nanoTime.flatMap { start =>
          handler.run(req).tap { res =>
            Clock.nanoTime.flatMap { end =>
              val ms  = (end - start) / 1_000_000
              val rid = req.header(HeaderName.XRequestId).getOrElse("-")
              val len = res.body.length.map(_.toString).getOrElse("-")
              ZIO.logAnnotate("method", req.method.render) {
                ZIO.logAnnotate("path", req.url.path.render) {
                  ZIO.logAnnotate("status", res.status.code.toString) {
                    ZIO.logAnnotate("request_id", rid) {
                      ZIO.logInfo(s"${req.method.render} ${req.url.path.render} ${res.status.code} ${ms}ms bytes=$len")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

  def serveDirectory(urlPrefix: String, root: String, indexHtml: Boolean): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        if req.method == Method.GET || req.method == Method.HEAD then
          heddle.server.Files
            .fromDirectory(root, urlPrefix, req, indexHtml)
            .foldZIO(
              _ => handler.run(req),
              {
                case Some(res) => ZIO.succeed(res)
                case None      => handler.run(req)
              },
            )
        else handler.run(req)
      }
    }

  def serveResources(urlPrefix: String, resourceRoot: String = ""): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        if req.method == Method.GET || req.method == Method.HEAD then
          SafePath.remainder(urlPrefix, req.path).flatMap { rest =>
            SafePath.resolveUnder(resourceRoot, rest)
          } match
            case None       => handler.run(req)
            case Some(name) =>
              Files
                .fromResource(name, req)
                .foldZIO(
                  _ => handler.run(req),
                  {
                    case Some(res) => ZIO.succeed(res)
                    case None      => handler.run(req)
                  },
                )
        else handler.run(req)
      }
    }

  def requestId(): Middleware[Any] = requestId("X-Request-Id")

  def requestId(headerName: String): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        req.header(headerName).fold(heddle.internal.Ids.uuid.map(_.toString))(ZIO.succeed(_)).flatMap { id =>
          handler.run(req.withHeader(headerName, id)).map(_.withHeader(headerName, id))
        }
      }
    }

  def compress(
      minBytes: Int = 1024,
      compressors: Chunk[Compressor] = Chunk(Compressor.gzip),
  ): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        handler.run(req).map(res => applyCompress(req, res, minBytes, compressors))
      }
    }

  def decompress(
      maxBytes: BytesLength = Server.Config.defaultMaxBodyBytes,
      decompressors: Chunk[Decompressor] = Chunk(Decompressor.gzip),
  ): Middleware[Any] =
    interceptZIO { req =>
      req.headers.contentEncoding.filter(_ != ContentEncoding.Identity) match
        case Chunk() => ZIO.succeed(req)
        case codings =>
          // RFC 9110 §15.5.16: a coding this server cannot decode is 415, never a compressed body handed on as plain.
          (if codings.length == 1 then codings.headOption else None).flatMap(c =>
            decompressors.find(_.encoding == c)
          ) match
            case None    => ZIO.fail(Response.text("Unsupported Content-Encoding", Status.UnsupportedMediaType))
            case Some(d) =>
              req.body.collect
                .orElseFail(Response.badRequest("Unreadable body"))
                .flatMap { bytes =>
                  d.decompress(bytes, maxBytes) match
                    case Left(HttpError.BodyTooLarge) =>
                      ZIO.fail(Response.text("Decompressed body too large", Status.ContentTooLarge))
                    case Left(_)    => ZIO.fail(Response.badRequest("Invalid Content-Encoding"))
                    case Right(out) =>
                      ZIO.succeed(
                        req.copy(
                          headers = req.headers.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength),
                          body = Body.fromBytes(out, req.body.mediaType),
                        )
                      )
                }
    }

  def requireTls: Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        if req.secure then handler.run(req)
        else ZIO.succeed(Response.empty(Status.Forbidden).withHeader("Connection", "close"))
      }
    }

  def cors(): Middleware[Any] = cors(CorsConfig())

  /** CORS (Fetch §3.2). A preflight is an `OPTIONS` naming `Access-Control-Request-Method`; any other request is
    * handled, then answered with the origin policy.
    */
  def cors(config: CorsConfig): Middleware[Any] =
    wrap[Any] { [R1, E] => (handler: Handler[R1, E]) =>
      Handler { req =>
        val origin = req.headers.get(HeaderName.Origin)
        if req.method == Method.OPTIONS && req.headers.get(HeaderName.AccessControlRequestMethod).isDefined then
          ZIO.succeed(preflight(origin, config))
        else handler.run(req).map(allowOrigin(_, origin, config.origins))
      }
    }

  /** Which origins may read responses. */
  enum CorsOrigins:
    /** Every origin, answered with `*`. Browsers refuse credentials with `*`, so this policy offers none. */
    case Any

    /** Exactly these serialized origins (`https://app.example`), echoed back with `Vary: Origin`. */
    case Only(origins: Set[String], credentials: CorsCredentials = CorsCredentials.Omit)

  /** Whether a listed origin may send cookies and `Authorization` and read the answer. */
  enum CorsCredentials:
    case Omit, Include

  final case class CorsConfig(
      origins: CorsOrigins = CorsOrigins.Any,
      methods: Set[Method] =
        Set(Method.GET, Method.POST, Method.PUT, Method.PATCH, Method.DELETE, Method.OPTIONS, Method.HEAD),
      headers: Set[HeaderName] = Set(HeaderName.ContentType, HeaderName.Authorization, HeaderName("X-Request-Id")),
      maxAge: Duration = 1.day,
  )

  private def applyCompress(
      req: Request,
      res: Response,
      minBytes: Int,
      compressors: Chunk[Compressor],
  ): Response =
    if skipCompress(res) then res
    else
      pick(req, compressors) match
        case None    => res
        case Some(c) =>
          val small = res.body.length.exists(_ < minBytes) && res.body.length.exists(_ >= 0)
          if small then res
          else
            val next = res.body match
              case Body.Empty            => res
              case Body.Bytes(bytes, ct) =>
                if bytes.length < minBytes then res
                else
                  val out = c.compress(bytes)
                  res
                    .withBody(Body.fromBytes(out, ct))
                    .withHeader(HeaderName.ContentEncoding, c.encoding.token)
                    .removeLength
              case Body.Stream(s, ct, len) =>
                if len.exists(_ < minBytes) then res
                else
                  res
                    .withBody(Body.stream(c.stream(s), ct, None))
                    .withHeader(HeaderName.ContentEncoding, c.encoding.token)
                    .removeLength
            next
          end if

  private def skipCompress(res: Response): Boolean =
    val ct  = res.headers.contentType.orElse(res.body.mediaType)
    val enc = res.headers.get(HeaderName.ContentEncoding)
    res.status == Status.SwitchingProtocols
    || enc.exists(_.nonEmpty)
    || ct.exists(_.isEventStream)
    || ct.exists(_.isImage)
    || ct.exists(_.isVideo)
    || ct.exists(m => m.mainType.contains("zip") || m.subType.contains("zip"))
    || res.headers.get(HeaderName.Upgrade).exists(_.toLowerCase.contains("websocket"))
  end skipCompress

  private def pick(req: Request, compressors: Chunk[Compressor]): Option[Compressor] =
    ContentEncoding
      .negotiate(req.headers.get(HeaderName.AcceptEncoding), compressors.map(_.encoding))
      .flatMap(e => compressors.find(_.encoding == e))

  extension (res: Response)
    private def removeLength: Response =
      res.copy(headers = res.headers.remove(HeaderName.ContentLength))

  private def wrap[R](f: [R1 <: R, E] => Handler[R1, E] => Handler[R1, E]): Middleware[R] =
    new Middleware[R]:
      def apply[R1 <: R, E](routes: Routes[R1, E]): Routes[R1, E] =
        Routes.wrap(routes)(f[R1, E])

  private def preflight(origin: Option[String], config: CorsConfig): Response =
    allowOrigin(Response.empty(Status.NoContent), origin, config.origins)
      .withHeader(HeaderName.AccessControlAllowMethods, config.methods.toList.map(_.render).sorted.mkString(", "))
      .withHeader(HeaderName.AccessControlAllowHeaders, config.headers.toList.map(_.render).sorted.mkString(", "))
      .withHeader(HeaderName.AccessControlMaxAge, config.maxAge.toSeconds.toString)

  /** `*` for any origin; a listed origin echoed, with `Vary: Origin` either way so a cache never hands one origin's
    * answer to another.
    */
  private def allowOrigin(response: Response, origin: Option[String], origins: CorsOrigins): Response =
    origins match
      case CorsOrigins.Any                  => response.withHeader(HeaderName.AccessControlAllowOrigin, "*")
      case CorsOrigins.Only(allowed, creds) =>
        val varied = response.withHeader(HeaderName.Vary, HeaderName.Origin.render)
        origin.filter(allowed.contains) match
          case None    => varied
          case Some(o) =>
            val granted = varied.withHeader(HeaderName.AccessControlAllowOrigin, o)
            creds match
              case CorsCredentials.Include => granted.withHeader(HeaderName.AccessControlAllowCredentials, "true")
              case CorsCredentials.Omit    => granted
end Middleware
