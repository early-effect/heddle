package heddle.endpoint

import scala.deriving.Mirror
import heddle.client.CallFailure
import heddle.error.ParamError
import heddle.http.{Body, Method, Path, QueryParams, Request, Response, Status, Url}
import heddle.http.header.Headers
import heddle.http.header.TypedHeader
import heddle.route.{Combiner, HeaderCodec, PathCodec, PathKind, QueryCodec, Route, Routes}
import zio.{IO, ZIO}

sealed abstract class Endpoint[In, Err, Out]:
  type PathIn
  def method: Method
  def path: PathCodec[PathIn]
  def decodeIn: (PathIn, Request) => IO[Response, In]
  def encodeOut: Out => Response

  /** Reads a success body back into `Out`. Set by the builder that chose the output, so no content type is guessed. */
  def decodeOut: Response => IO[BodyError, Out]
  def errors: ErrorCodec[Err]

  /** The success body, when this endpoint has one. Empty and the initial placeholder have none. */
  private[heddle] def outputBody: Option[BodyCodec[Out]]
  def doc: EndpointDoc

  /** The inverse of `decodeIn`: the path value and the query, headers, and body that decode back to `in`. */
  def encodeIn: In => (PathIn, Endpoint.Acc)

  def encodeErr(e: Err): Response = errors.encode(e)

  def query[Q](name: String)(using codec: QueryCodec[Q], c: Combiner[In, Q]): Endpoint[c.Out, Err, Out] =
    adding(c)(
      req => ZIO.fromEither(codec.decode(req.query.getAll(name))).mapError(Endpoint.refused(s"query parameter $name")),
      doc.copy(queries = doc.queries :+ ParamDoc(name, ParamLocation.Query, codec.required, codec.schema)),
      (q, acc) => acc.copy(query = acc.query.add(name, codec.encode(q))),
    )

  def header[H](name: String)(using codec: HeaderCodec[H], c: Combiner[In, H]): Endpoint[c.Out, Err, Out] =
    adding(c)(
      req => ZIO.fromEither(codec.decode(req.header(name))).mapError(Endpoint.refused(s"header $name")),
      doc.copy(headers = doc.headers :+ ParamDoc(name, ParamLocation.Header, codec.required, codec.schema)),
      (h, acc) => codec.encode(h).fold(acc)(v => acc.copy(headers = acc.headers.add(name, v))),
    )

  def header[A](using t: TypedHeader[A], c: Combiner[In, A]): Endpoint[c.Out, Err, Out] =
    adding(c)(
      req =>
        ZIO
          .fromEither(req.header(t.name).toRight(ParamError.Missing).flatMap(t.decode))
          .mapError(Endpoint.refused(s"header ${t.name.render}")),
      doc.copy(headers =
        doc.headers :+ ParamDoc(t.name.render, ParamLocation.Header, required = true, SchemaDoc.Str(None))
      ),
      (a, acc) => acc.copy(headers = acc.headers.add(t.name, t.encode(a))),
    )

  def in[B](using codec: BodyCodec[B], c: Combiner[In, B]): Endpoint[c.Out, Err, Out] =
    adding(c)(
      req => codec.decode(req.body).mapError(e => Response.badRequest(e.message)),
      doc.copy(requestBody = Some(MediaDoc(codec.schema, codec.mediaType))),
      (b, acc) => acc.copy(body = BodyCodec.payload(codec, b)),
    )

  def out[O: BodyCodec]: Endpoint[In, Err, O] =
    out(Status.Ok)

  def out[O](status: Status)(using codec: BodyCodec[O]): Endpoint[In, Err, O] =
    replace(
      encodeOut = o => BodyCodec.response(status, codec, o),
      decodeOut = res => codec.decode(res.body),
      errors = errors,
      doc = doc.copy(responses =
        Endpoint.replaceSuccess(
          doc.responses,
          StatusDoc(status, Some(codec.schema), Some(codec.mediaType), status.text),
        )
      ),
      outputBody = Some(codec),
    )

  def outEmpty: Endpoint[In, Err, Unit] =
    outEmpty(Status.NoContent)

  def outEmpty(status: Status): Endpoint[In, Err, Unit] =
    replace(
      encodeOut = (_: Unit) => Response.empty(status),
      decodeOut = _ => ZIO.unit,
      errors = errors,
      doc = doc.copy(responses = Endpoint.replaceSuccess(doc.responses, StatusDoc(status, None, None, status.text))),
      outputBody = None,
    )

  /** Every error answers with `status`. For a sealed hierarchy with a status per case, use `outErrors`. */
  def outError[E1: BodyCodec](status: Status): Endpoint[In, E1, Out] =
    withErrors(ErrorCodec.single[E1](status))

  /** Pins the error ADT `E`; the returned builder takes one `ErrorCase` per case, checked at compile time. */
  def outErrors[E]: Endpoint.OutErrors[In, E, Out] =
    Endpoint.OutErrors(this)

  private[endpoint] def withErrors[E1](next: ErrorCodec[E1]): Endpoint[In, E1, Out] =
    val claimed   = (errors.statuses ++ next.statuses).map(_.code)
    val kept      = doc.responses.filterNot(r => claimed.contains(r.status.code)) ++ next.docs
    val responses =
      if doc.security.isEmpty || kept.exists(_.status == Status.Unauthorized) then kept
      else kept :+ Endpoint.unauthorized
    replace(encodeOut, decodeOut, next, doc.copy(responses = responses), outputBody)

  def name(id: String): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(operationId = Some(id)))

  def summary(text: String): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(summary = Some(text)))

  def description(text: String): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(description = Some(text)))

  def tag(tagName: String): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(tags = doc.tags :+ tagName))

  def mcp: Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(promoted = true))

  def mcp(name: String): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(promoted = true, mcpName = Some(name)))

  def unpromote: Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(promoted = false))

  def nestBody: Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(nestBody = true))

  def hints(h: Hint*): Endpoint[In, Err, Out] =
    replaceDoc(doc.copy(hints = doc.hints ++ h.toList))

  def auth(scheme: SecurityScheme, extra: SecurityScheme*): Endpoint[In, Err, Out] =
    val schemes = scheme :: extra.toList
    val docs    =
      if doc.responses.exists(_.status == Status.Unauthorized) then doc.responses
      else doc.responses :+ Endpoint.unauthorized
    replaceDoc(doc.copy(security = doc.security ++ schemes, responses = docs))

  /** Renames the input. `from` must undo `to`, so a typed client can still build the request. */
  def mapIn[B](to: In => B)(from: B => In): Endpoint[B, Err, Out] =
    Endpoint.Impl(
      method,
      path,
      (pathIn, req) => decodeIn(pathIn, req).map(to),
      encodeIn.compose(from),
      encodeOut,
      decodeOut,
      errors,
      outputBody,
      doc,
    )

  /** Names the input as the case class whose fields are its values, in order, both ways. */
  def as[B <: Product](using m: Mirror.ProductOf[B])(using same: m.MirroredElemTypes =:= In): Endpoint[B, Err, Out] =
    mapIn(in => m.fromTuple(same.flip(in)))(b => same(Tuple.fromProductTyped(b)(using m)))

  /** The request `decodeIn` reads back as `in`. Total: every builder records how to put its piece back. */
  def toRequest(in: In, base: Url): Request =
    val (pathIn, acc) = encodeIn(in)
    val url           = base.copy(path = Path(base.path.segments ++ path.encode(pathIn).segments), query = acc.query)
    Request(method, url, acc.headers, acc.body)

  def fromResponse(res: Response): IO[CallFailure[Err], Out] =
    if errors.handles(res.status) then decodeFailure(res)
    else if res.status.isSuccess then decodeOut(res).mapError(CallFailure.Undecodable(res.status, _))
    else ZIO.fail(CallFailure.Unexpected(res.status))

  private def decodeFailure(res: Response): IO[CallFailure[Err], Out] =
    errors.decode(res.status, res.body).mapError(CallFailure.Undecodable(res.status, _)).flatMap {
      case None    => ZIO.fail(CallFailure.Unexpected(res.status))
      case Some(e) => ZIO.fail(CallFailure.Domain(e))
    }

  /** Adds one input: `read` decodes it from the request, `c` joins it to what came before, and `put` writes it back. */
  private def adding[P](c: Combiner[In, P])(
      read: Request => IO[Response, P],
      nextDoc: EndpointDoc,
      put: (P, Endpoint.Acc) => Endpoint.Acc,
  ): Endpoint[c.Out, Err, Out] =
    Endpoint.Impl(
      method,
      path,
      (pathIn, req) => decodeIn(pathIn, req).flatMap(in => read(req).map(c.combine(in, _))),
      out =>
        val (in, piece)   = c.separate(out)
        val (pathIn, acc) = encodeIn(in)
        (pathIn, put(piece, acc))
      ,
      encodeOut,
      decodeOut,
      errors,
      outputBody,
      nextDoc,
    )

  private def replace[E1, O1](
      encodeOut: O1 => Response,
      decodeOut: Response => IO[BodyError, O1],
      errors: ErrorCodec[E1],
      doc: EndpointDoc,
      outputBody: Option[BodyCodec[O1]],
  ): Endpoint[In, E1, O1] =
    Endpoint.Impl(method, path, decodeIn, encodeIn, encodeOut, decodeOut, errors, outputBody, doc)

  private def replaceDoc(next: EndpointDoc): Endpoint[In, Err, Out] =
    replace(encodeOut, decodeOut, errors, next, outputBody)

  def implement[R](f: In => ZIO[R, Err, Out]): BoundOp[R, In, Err, Out] =
    val routes = Routes(
      Route.from(
        method,
        path,
        (pathIn, req) =>
          decodeIn(pathIn, req).foldZIO(
            res => ZIO.succeed(res),
            in => f(in).fold(errors.encode, encodeOut),
          ),
      )
    )
    BoundOp(this, f, routes)
  end implement

end Endpoint

object Endpoint:
  inline def get[N <: Tuple, A](inline path: PathCodec[A] { type Names = N }): Endpoint[A, Nothing, Unit] =
    make(Method.GET, PathCodec.specialize(path))
  inline def get(inline path: String): Endpoint[Unit, Nothing, Unit] =
    get(PathCodec.lit(path))
  inline def post[N <: Tuple, A](inline path: PathCodec[A] { type Names = N }): Endpoint[A, Nothing, Unit] =
    make(Method.POST, PathCodec.specialize(path))
  inline def post(inline path: String): Endpoint[Unit, Nothing, Unit] =
    post(PathCodec.lit(path))
  inline def put[N <: Tuple, A](inline path: PathCodec[A] { type Names = N }): Endpoint[A, Nothing, Unit] =
    make(Method.PUT, PathCodec.specialize(path))
  inline def put(inline path: String): Endpoint[Unit, Nothing, Unit] =
    put(PathCodec.lit(path))
  inline def patch[N <: Tuple, A](inline path: PathCodec[A] { type Names = N }): Endpoint[A, Nothing, Unit] =
    make(Method.PATCH, PathCodec.specialize(path))
  inline def patch(inline path: String): Endpoint[Unit, Nothing, Unit] =
    patch(PathCodec.lit(path))
  inline def delete[N <: Tuple, A](inline path: PathCodec[A] { type Names = N }): Endpoint[A, Nothing, Unit] =
    make(Method.DELETE, PathCodec.specialize(path))
  inline def delete(inline path: String): Endpoint[Unit, Nothing, Unit] =
    delete(PathCodec.lit(path))

  private def make[A](method: Method, path: PathCodec[A]): Endpoint[A, Nothing, Unit] =
    Impl(
      method,
      path,
      (pathIn, _) => ZIO.succeed(pathIn),
      pathIn => (pathIn, Acc.empty),
      _ => Response.empty(Status.NoContent),
      _ => ZIO.unit,
      ErrorCodec.none,
      None,
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

  final class OutErrors[In, E, Out] private[endpoint] (endpoint: Endpoint[In, ?, Out]):
    inline def apply(inline cases: ErrorCase[? <: E]*)(using s: Schema[E], b: BodyCodec[E]): Endpoint[In, E, Out] =
      endpoint.withErrors(ErrorCaseMacros.codec[E](cases*)(using s, b))

  final case class Acc(query: QueryParams, headers: Headers, body: Body)

  object Acc:
    val empty: Acc = Acc(QueryParams.empty, Headers.empty, Body.empty)

  private final class Impl[P, In, Err, Out](
      val method: Method,
      val path: PathCodec[P],
      val decodeIn: (P, Request) => IO[Response, In],
      val encodeIn: In => (P, Acc),
      val encodeOut: Out => Response,
      val decodeOut: Response => IO[BodyError, Out],
      val errors: ErrorCodec[Err],
      val outputBody: Option[BodyCodec[Out]],
      val doc: EndpointDoc,
  ) extends Endpoint[In, Err, Out]:
    type PathIn = P
  end Impl

  /** A 400 that names the parameter and why it was refused: `query parameter n: expected an int, got 'x'`. */
  private[endpoint] def refused(what: String)(e: ParamError): Response =
    Response.badRequest(s"$what: ${e.message}")

  private def paramDoc(pair: (String, PathKind)): ParamDoc =
    val (name, kind) = pair
    val schema       = kind match
      case PathKind.Int32 => SchemaDoc.Integer(Some("int32"))
      case PathKind.Int64 => SchemaDoc.Integer(Some("int64"))
      case PathKind.Str   => SchemaDoc.Str(None)
      case PathKind.Uuid  => SchemaDoc.Str(Some("uuid"))
    ParamDoc(name, ParamLocation.Path, required = true, schema)

  private[heddle] def replaceSuccess(responses: List[StatusDoc], next: StatusDoc): List[StatusDoc] =
    val withoutPlaceholder = responses.filterNot(r => r.status == Status.NoContent && r.schema.isEmpty)
    next :: withoutPlaceholder.filterNot(_.status == next.status)

  private val unauthorized: StatusDoc = StatusDoc(Status.Unauthorized, None, None, "Unauthorized")
end Endpoint
