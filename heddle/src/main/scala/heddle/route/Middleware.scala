package heddle.route

import heddle.auth.Auth
import heddle.endpoint.ApiKeyIn
import heddle.BytesLength
import heddle.http.{Body, ContentEncoding, Method, Request, Response, Status}
import heddle.http.header.{BasicCredentials, HeaderName}
import heddle.server.{Compressor, Decompressor, Files}
import heddle.Server
import java.util.UUID
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
    wrap[R] { [E] => (handler: Handler[R, E]) =>
      Handler { req =>
        f(req).foldZIO(res => ZIO.succeed(res), handler.run)
      }
    }

  def mapResponse(f: Response => Response): Middleware[Any] =
    mapResponseZIO(res => ZIO.succeed(f(res)))

  def mapResponseZIO[R](f: Response => ZIO[R, Nothing, Response]): Middleware[R] =
    wrap[R] { [E] => (handler: Handler[R, E]) =>
      Handler { req =>
        handler.run(req).flatMap(f)
      }
    }

  def debug: Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
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
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        handler.run(req).timeout(duration).map {
          case Some(res) => res
          case None      => Response.empty(Status.GatewayTimeout)
        }
      }
    }

  def requestLog: Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
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

  def serveDirectory(urlPrefix: String, root: java.nio.file.Path, indexHtml: Boolean = true): Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        if req.method == Method.GET || req.method == Method.HEAD then
          Files
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
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        if req.method == Method.GET || req.method == Method.HEAD then
          val prefix = if urlPrefix.startsWith("/") then urlPrefix else s"/$urlPrefix"
          val path   = req.path.render
          val rel    =
            if path == prefix then ""
            else if path.startsWith(prefix + "/") then path.substring(prefix.length + 1)
            else ""
          val name = List(resourceRoot.stripSuffix("/"), rel).filter(_.nonEmpty).mkString("/")
          if rel.nonEmpty || path == prefix then
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
          end if
        else handler.run(req)
      }
    }

  def requestId(headerName: String = "X-Request-Id"): Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        val id     = req.header(headerName).getOrElse(UUID.randomUUID().toString)
        val tagged = req.withHeader(headerName, id)
        handler.run(tagged).map(_.withHeader(headerName, id))
      }
    }

  def compress(
      minBytes: Int = 1024,
      compressors: Chunk[Compressor] = Chunk(Compressor.gzip),
  ): Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        handler.run(req).map(res => applyCompress(req, res, minBytes, compressors))
      }
    }

  def decompress(
      maxBytes: BytesLength = Server.Config.defaultMaxBodyBytes,
      decompressors: Chunk[Decompressor] = Chunk(Decompressor.gzip),
  ): Middleware[Any] =
    interceptZIO { req =>
      req.header("Content-Encoding").map(_.trim.toLowerCase).filter(_.nonEmpty) match
        case None | Some("identity") => ZIO.succeed(req)
        case Some(token)             =>
          decompressors.find(_.encoding.token == token) match
            case None    => ZIO.succeed(req)
            case Some(d) =>
              req.body.collect
                .flatMap(bytes => ZIO.attempt(d.decompress(bytes)))
                .foldZIO(
                  _ => ZIO.fail(Response.badRequest("Invalid Content-Encoding")),
                  out =>
                    if out.length > maxBytes.toLong then
                      ZIO.fail(Response.text("Decompressed body too large", Status.ContentTooLarge))
                    else
                      ZIO.succeed(
                        req.copy(
                          headers = req.headers.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength),
                          body = Body.fromBytes(out, req.body.mediaType),
                        )
                      ),
                )
    }

  def requireTls: Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        if req.secure then handler.run(req)
        else ZIO.succeed(Response.empty(Status.Forbidden).withHeader("Connection", "close"))
      }
    }

  def cors(config: CorsConfig = CorsConfig()): Middleware[Any] =
    wrap[Any] { [E] => (handler: Handler[Any, E]) =>
      Handler { req =>
        val origin = req.header("Origin")
        if req.method == Method.OPTIONS then ZIO.succeed(preflight(origin, config))
        else handler.run(req).map(addCors(_, origin, config))
      }
    }

  final case class CorsConfig(
      allowOrigin: String = "*",
      allowMethods: String = "GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD",
      allowHeaders: String = "Content-Type, Authorization, X-Request-Id",
      allowCredentials: Boolean = false,
      maxAgeSeconds: Int = 86400,
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
    val offered = req.headers.acceptEncoding
    if offered.contains(ContentEncoding.Other) then compressors.headOption
    else offered.flatMap(enc => compressors.find(_.encoding == enc)).headOption

  extension (res: Response)
    private def removeLength: Response =
      res.copy(headers = res.headers.remove(HeaderName.ContentLength))

  private def wrap[R](f: [E] => Handler[R, E] => Handler[R, E]): Middleware[R] =
    new Middleware[R]:
      def apply[R1 <: R, E](routes: Routes[R1, E]): Routes[R1, E] =
        Routes.wrap(routes) { handler =>
          f[E](handler.asInstanceOf[Handler[R, E]]).asInstanceOf[Handler[R1, E]]
        }

  private def preflight(origin: Option[String], config: CorsConfig): Response =
    addCors(Response.empty(Status.NoContent), origin, config)
      .withHeader("Access-Control-Allow-Methods", config.allowMethods)
      .withHeader("Access-Control-Allow-Headers", config.allowHeaders)
      .withHeader("Access-Control-Max-Age", config.maxAgeSeconds.toString)

  private def addCors(response: Response, origin: Option[String], config: CorsConfig): Response =
    val allow =
      if config.allowOrigin == "*" then origin.getOrElse("*")
      else config.allowOrigin
    val withOrigin = response.withHeader("Access-Control-Allow-Origin", allow)
    if config.allowCredentials then withOrigin.withHeader("Access-Control-Allow-Credentials", "true")
    else withOrigin
end Middleware
