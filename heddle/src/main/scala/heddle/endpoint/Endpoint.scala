package heddle.endpoint

import heddle.http.{Body, Form, MediaType, Method, Request, Response, Status}
import heddle.http.header.TypedHeader
import heddle.route.{Combine, HeaderCodec, PathCodec, PathKind, QueryCodec, Route, Routes}
import heddle.sse.{ServerSentEvent, Sse}
import zio.stream.ZStream
import zio.{Chunk, IO, ZIO}

sealed abstract class Endpoint[In, Err, Out]:
  type PathIn
  def method: Method
  def path: PathCodec[PathIn]
  def decodeIn: (PathIn, Request) => IO[Response, In]
  def encodeOut: Out => Response
  def encodeErr: Err => Response
  def doc: EndpointDoc

  def query[Q](name: String)(using codec: QueryCodec[Q]): Endpoint[Combine[In, Q], Err, Out] =
    bindSync(
      (in, req) => codec.decode(req.query.getAll(name)).left.map(Response.badRequest).map(q => Combine(in, q)),
      doc.copy(queries = doc.queries :+ ParamDoc(name, "query", codec.required, codec.schema)),
    )

  def header[H](name: String)(using codec: HeaderCodec[H]): Endpoint[Combine[In, H], Err, Out] =
    bindSync(
      (in, req) => codec.decode(req.header(name)).left.map(Response.badRequest).map(h => Combine(in, h)),
      doc.copy(headers = doc.headers :+ ParamDoc(name, "header", codec.required, codec.schema)),
    )

  def header[A](using t: TypedHeader[A]): Endpoint[Combine[In, A], Err, Out] =
    bindSync(
      (in, req) =>
        req.header(t.name) match
          case None      => Left(Response.badRequest("missing header"))
          case Some(raw) => t.decode(raw).left.map(Response.badRequest).map(a => Combine(in, a))
      ,
      doc.copy(headers = doc.headers :+ ParamDoc(t.name.render, "header", required = true, SchemaDoc.Str(None))),
    )

  def inJson[B](using s: Schema[B], j: JsonCodec[B]): Endpoint[Combine[In, B], Err, Out] =
    bindNext(
      (in, req) =>
        req.body.collect
          .mapError(e => Response.badRequest(e.getMessage))
          .flatMap { raw =>
            j.decodeBytes(raw) match
              case Left(msg) => ZIO.fail(Response.badRequest(msg))
              case Right(b)  => ZIO.succeed(Combine(in, b))
          },
      doc.copy(requestBody = Some(MediaDoc(s.doc, MediaType.Json))),
    )

  def inText: Endpoint[Combine[In, String], Err, Out] =
    bindNext(
      (in, req) =>
        req.body.collect
          .map(c => String(c.toArray, java.nio.charset.StandardCharsets.UTF_8))
          .mapBoth(e => Response.badRequest(e.getMessage), raw => Combine(in, raw)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(None), MediaType.Text))),
    )

  def inForm: Endpoint[Combine[In, Form], Err, Out] =
    bindNext(
      (in, req) => req.body.asForm.mapBoth(e => Response.badRequest(e.getMessage), form => Combine(in, form)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(None), MediaType.FormUrlEncoded))),
    )

  def inBytes: Endpoint[Combine[In, Chunk[Byte]], Err, Out] =
    bindNext(
      (in, req) => req.body.collect.mapBoth(e => Response.badRequest(e.getMessage), b => Combine(in, b)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(Some("binary")), MediaType.OctetStream))),
    )

  def out[O](using s: Schema[O], j: JsonCodec[O]): Endpoint[In, Err, O] =
    out(Status.Ok)

  def out[O](status: Status)(using s: Schema[O], j: JsonCodec[O]): Endpoint[In, Err, O] =
    replace(
      encodeOut = o => Response(status).withBody(Body.fromBytes(j.encodeBytes(o), Some(MediaType.Json))),
      encodeErr = encodeErr,
      doc = doc.copy(responses = Endpoint.replaceSuccess(doc.responses, jsonStatus(status, s.doc))),
    )

  def outText(status: Status = Status.Ok): Endpoint[In, Err, String] =
    replace(
      encodeOut = s => Response.text(s, status),
      encodeErr = encodeErr,
      doc = doc.copy(responses =
        Endpoint.replaceSuccess(
          doc.responses,
          StatusDoc(status, Some(SchemaDoc.Str(None)), Some(MediaType.Text), status.text),
        )
      ),
    )

  def outSse: Endpoint[In, Err, ZStream[Any, Throwable, ServerSentEvent]] =
    replace(
      encodeOut = Sse.response,
      encodeErr = encodeErr,
      doc = doc.copy(responses =
        Endpoint.replaceSuccess(
          doc.responses,
          StatusDoc(Status.Ok, Some(SchemaDoc.Str(None)), Some(MediaType.EventStream), "Server-Sent Events"),
        )
      ),
    )

  def outEmpty(status: Status = Status.NoContent): Endpoint[In, Err, Unit] =
    replace(
      encodeOut = (_: Unit) => Response.empty(status),
      encodeErr = encodeErr,
      doc = doc.copy(responses = Endpoint.replaceSuccess(doc.responses, StatusDoc(status, None, None, status.text))),
    )

  def outError[E1](status: Status)(using s: Schema[E1], j: JsonCodec[E1]): Endpoint[In, E1, Out] =
    replace(
      encodeOut = encodeOut,
      encodeErr = e => Response(status).withBody(Body.fromBytes(j.encodeBytes(e), Some(MediaType.Json))),
      doc = doc.copy(responses = doc.responses :+ jsonStatus(status, s.doc)),
    )

  def name(id: String): Endpoint[In, Err, Out] =
    replace(encodeOut, encodeErr, doc.copy(operationId = Some(id)))

  def summary(text: String): Endpoint[In, Err, Out] =
    replace(encodeOut, encodeErr, doc.copy(summary = Some(text)))

  def description(text: String): Endpoint[In, Err, Out] =
    replace(encodeOut, encodeErr, doc.copy(description = Some(text)))

  def tag(tagName: String): Endpoint[In, Err, Out] =
    replace(encodeOut, encodeErr, doc.copy(tags = doc.tags :+ tagName))

  def auth(scheme: SecurityScheme, extra: SecurityScheme*): Endpoint[In, Err, Out] =
    val schemes = scheme :: extra.toList
    val docs    =
      if doc.responses.exists(_.status == Status.Unauthorized) then doc.responses
      else doc.responses :+ StatusDoc(Status.Unauthorized, None, None, "Unauthorized")
    replace(encodeOut, encodeErr, doc.copy(security = doc.security ++ schemes, responses = docs))

  def mapIn[B](f: In => B): Endpoint[B, Err, Out] =
    bindSync((in, _) => Right(f(in)), doc)

  private def bindSync[N](
      next: (In, Request) => Either[Response, N],
      nextDoc: EndpointDoc,
  ): Endpoint[N, Err, Out] =
    Endpoint.Impl(
      method,
      path,
      (pathIn, req) =>
        decodeIn(pathIn, req).flatMap { in =>
          next(in, req) match
            case Left(res) => ZIO.fail(res)
            case Right(n)  => ZIO.succeed(n)
        },
      encodeOut,
      encodeErr,
      nextDoc,
    )

  private def bindNext[N](
      next: (In, Request) => IO[Response, N],
      nextDoc: EndpointDoc,
  ): Endpoint[N, Err, Out] =
    Endpoint.Impl(
      method,
      path,
      (pathIn, req) => decodeIn(pathIn, req).flatMap(in => next(in, req)),
      encodeOut,
      encodeErr,
      nextDoc,
    )

  private def replace[E1, O1](
      encodeOut: O1 => Response,
      encodeErr: E1 => Response,
      doc: EndpointDoc,
  ): Endpoint[In, E1, O1] =
    Endpoint.Impl(method, path, decodeIn, encodeOut, encodeErr, doc)

  def implement[R](f: In => ZIO[R, Err, Out]): Routes[R, Response] =
    Routes(
      Route.from(
        method,
        path,
        (pathIn, req) =>
          decodeIn(pathIn, req).foldZIO(
            res => ZIO.succeed(res),
            in => f(in).fold(encodeErr, encodeOut),
          ),
      )
    )

  private def jsonStatus(status: Status, schema: SchemaDoc): StatusDoc =
    StatusDoc(status, Some(schema), Some(MediaType.Json), status.text)
end Endpoint

object Endpoint:
  inline def get[A](inline path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    make(Method.GET, PathCodec.specialize(path))
  inline def get(inline path: String): Endpoint[Unit, Nothing, Unit] =
    get(PathCodec.lit(path))
  inline def post[A](inline path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    make(Method.POST, PathCodec.specialize(path))
  inline def post(inline path: String): Endpoint[Unit, Nothing, Unit] =
    post(PathCodec.lit(path))
  inline def put[A](inline path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    make(Method.PUT, PathCodec.specialize(path))
  inline def put(inline path: String): Endpoint[Unit, Nothing, Unit] =
    put(PathCodec.lit(path))
  inline def patch[A](inline path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    make(Method.PATCH, PathCodec.specialize(path))
  inline def patch(inline path: String): Endpoint[Unit, Nothing, Unit] =
    patch(PathCodec.lit(path))
  inline def delete[A](inline path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    make(Method.DELETE, PathCodec.specialize(path))
  inline def delete(inline path: String): Endpoint[Unit, Nothing, Unit] =
    delete(PathCodec.lit(path))

  private def make[A](method: Method, path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    Impl(
      method,
      path,
      (pathIn, _) => ZIO.succeed(pathIn),
      _ => Response.empty(Status.NoContent),
      (n: Nothing) => n,
      EndpointDoc(
        method = method,
        pathTemplate = path.template,
        pathParams = path.pathParams.toList.map(paramDoc),
        queries = Nil,
        headers = Nil,
        requestBody = None,
        responses = List(StatusDoc(Status.NoContent, None, None, "No Content")),
        operationId = None,
        summary = None,
        description = None,
        tags = Nil,
      ),
    )

  private final class Impl[P, In, Err, Out](
      val method: Method,
      val path: PathCodec[P],
      val decodeIn: (P, Request) => IO[Response, In],
      val encodeOut: Out => Response,
      val encodeErr: Err => Response,
      val doc: EndpointDoc,
  ) extends Endpoint[In, Err, Out]:
    type PathIn = P

  private def paramDoc(pair: (String, PathKind)): ParamDoc =
    val (name, kind) = pair
    val schema       = kind match
      case PathKind.Int32 => SchemaDoc.Integer(Some("int32"))
      case PathKind.Int64 => SchemaDoc.Integer(Some("int64"))
      case PathKind.Str   => SchemaDoc.Str(None)
      case PathKind.Uuid  => SchemaDoc.Str(Some("uuid"))
    ParamDoc(name, "path", required = true, schema)

  private[heddle] def replaceSuccess(responses: List[StatusDoc], next: StatusDoc): List[StatusDoc] =
    val withoutPlaceholder = responses.filterNot(r => r.status == Status.NoContent && r.schema.isEmpty)
    next :: withoutPlaceholder.filterNot(_.status == next.status)
end Endpoint
