package heddle.endpoint

import java.nio.charset.StandardCharsets
import heddle.client.CallFailure
import heddle.http.{Body, Form, MediaType, Method, Path, QueryParams, Request, Response, Status, Url}
import heddle.http.header.Headers
import heddle.http.header.TypedHeader
import heddle.route.{Combine, HeaderCodec, PathCodec, PathKind, QueryCodec, Route, Routes}
import heddle.sse.{ServerSentEvent, Sse, SseCodec}
import zio.json.JsonCodec
import zio.stream.ZStream
import zio.{Chunk, IO, ZIO}

sealed abstract class Endpoint[In, Err, Out]:
  type PathIn
  def method: Method
  def path: PathCodec[PathIn]
  def decodeIn: (PathIn, Request) => IO[Response, In]
  def encodeOut: Out => Response

  /** Reads a success body back into `Out`. Set by the builder that chose the output, so no content type is guessed. */
  def decodeOut: Response => IO[String, Out]
  def errors: ErrorCodec[Err]
  def outputCodec: Option[JsonCodec[Out]]
  def doc: EndpointDoc
  def slots: Chunk[Endpoint.InputSlot]

  def encodeErr(e: Err): Response = errors.encode(e)

  inline def query[Q](name: String)(using codec: QueryCodec[Q]): Endpoint[Combine[In, Q], Err, Out] =
    bindSync(
      (in, req) => codec.decode(req.query.getAll(name)).left.map(Response.badRequest).map(q => zipIn(in, q)),
      doc.copy(queries = doc.queries :+ ParamDoc(name, ParamLocation.Query, codec.required, codec.schema)),
      Endpoint.InputSlot.combine[In, Q] { (piece, acc) =>
        acc.copy(query = acc.query.add(name, codec.encode(piece.asInstanceOf[Q])))
      },
    )

  inline def header[H](name: String)(using codec: HeaderCodec[H]): Endpoint[Combine[In, H], Err, Out] =
    bindSync(
      (in, req) => codec.decode(req.header(name)).left.map(Response.badRequest).map(h => zipIn(in, h)),
      doc.copy(headers = doc.headers :+ ParamDoc(name, ParamLocation.Header, codec.required, codec.schema)),
      Endpoint.InputSlot.combine[In, H] { (piece, acc) =>
        codec.encode(piece.asInstanceOf[H]) match
          case None    => acc
          case Some(v) => acc.copy(headers = acc.headers.add(name, v))
      },
    )

  inline def header[A](using t: TypedHeader[A]): Endpoint[Combine[In, A], Err, Out] =
    bindSync(
      (in, req) =>
        req.header(t.name) match
          case None      => Left(Response.badRequest("missing header"))
          case Some(raw) => t.decode(raw).left.map(Response.badRequest).map(a => zipIn(in, a))
      ,
      doc.copy(headers =
        doc.headers :+ ParamDoc(t.name.render, ParamLocation.Header, required = true, SchemaDoc.Str(None))
      ),
      Endpoint.InputSlot.combine[In, A] { (piece, acc) =>
        acc.copy(headers = acc.headers.add(t.name, t.encode(piece.asInstanceOf[A])))
      },
    )

  inline def inJson[B](using s: Schema[B], codec: JsonCodec[B]): Endpoint[Combine[In, B], Err, Out] =
    bindNext(
      (in, req) =>
        req.body.collect
          .mapError(e => Response.badRequest(e.getMessage))
          .flatMap { raw =>
            Endpoint.jsonDecode(raw) match
              case Left(msg) => ZIO.fail(Response.badRequest(msg))
              case Right(b)  => ZIO.succeed(zipIn(in, b))
          },
      doc.copy(requestBody = Some(MediaDoc(s.doc, MediaType.Json))),
      Endpoint.InputSlot.combine[In, B] { (piece, acc) =>
        acc.copy(body = Body.fromBytes(Endpoint.jsonBytes(piece.asInstanceOf[B]), Some(MediaType.Json)))
      },
    )

  inline def inText: Endpoint[Combine[In, String], Err, Out] =
    bindNext(
      (in, req) =>
        req.body.collect
          .map(c => String(c.toArray, java.nio.charset.StandardCharsets.UTF_8))
          .mapBoth(e => Response.badRequest(e.getMessage), raw => zipIn(in, raw)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(None), MediaType.Text))),
      Endpoint.InputSlot.combine[In, String] { (piece, acc) =>
        acc.copy(body = Body.text(piece.asInstanceOf[String]))
      },
    )

  inline def inForm: Endpoint[Combine[In, Form], Err, Out] =
    bindNext(
      (in, req) => req.body.asForm.mapBoth(e => Response.badRequest(e.getMessage), form => zipIn(in, form)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(None), MediaType.FormUrlEncoded))),
      Endpoint.InputSlot.combine[In, Form] { (piece, acc) =>
        acc.copy(body = Body.form(piece.asInstanceOf[Form]))
      },
    )

  inline def inBytes: Endpoint[Combine[In, Chunk[Byte]], Err, Out] =
    bindNext(
      (in, req) => req.body.collect.mapBoth(e => Response.badRequest(e.getMessage), b => zipIn(in, b)),
      doc.copy(requestBody = Some(MediaDoc(SchemaDoc.Str(Some("binary")), MediaType.OctetStream))),
      Endpoint.InputSlot.combine[In, Chunk[Byte]] { (piece, acc) =>
        acc.copy(body = Body.fromBytes(piece.asInstanceOf[Chunk[Byte]], Some(MediaType.OctetStream)))
      },
    )

  def out[O](using Schema[O], JsonCodec[O]): Endpoint[In, Err, O] =
    out(Status.Ok)

  def out[O](status: Status)(using s: Schema[O], j: JsonCodec[O]): Endpoint[In, Err, O] =
    replace(
      encodeOut = o => Response(status).withBody(Body.fromBytes(Endpoint.jsonBytes(o), Some(MediaType.Json))),
      decodeOut = res => Endpoint.bytes(res).flatMap(raw => ZIO.fromEither(Endpoint.jsonDecode[O](raw))),
      errors = errors,
      doc = doc.copy(responses = Endpoint.replaceSuccess(doc.responses, jsonStatus(status, s.doc))),
      outputCodec = Some(j),
    )

  def outText(status: Status = Status.Ok): Endpoint[In, Err, String] =
    replace(
      encodeOut = s => Response.text(s, status),
      decodeOut = res => Endpoint.bytes(res).map(raw => String(raw.toArray, StandardCharsets.UTF_8)),
      errors = errors,
      doc = doc.copy(responses =
        Endpoint.replaceSuccess(
          doc.responses,
          StatusDoc(status, Some(SchemaDoc.Str(None)), Some(MediaType.Text), status.text),
        )
      ),
      outputCodec = Some(summon[JsonCodec[String]]),
    )

  /** A typed client reads this with `Client.subscribe`, not `Client.call`: the body is a stream. */
  def outSse: Endpoint[In, Err, ZStream[Any, Throwable, ServerSentEvent]] =
    replace(
      encodeOut = Sse.response,
      decodeOut = res => ZIO.succeed(SseCodec.stream(res.body.toStream)),
      errors = errors,
      doc = doc.copy(responses =
        Endpoint.replaceSuccess(
          doc.responses,
          StatusDoc(Status.Ok, Some(SchemaDoc.Str(None)), Some(MediaType.EventStream), "Server-Sent Events"),
        )
      ),
      outputCodec = None,
    )

  def outEmpty(status: Status = Status.NoContent): Endpoint[In, Err, Unit] =
    replace(
      encodeOut = (_: Unit) => Response.empty(status),
      decodeOut = _ => ZIO.unit,
      errors = errors,
      doc = doc.copy(responses = Endpoint.replaceSuccess(doc.responses, StatusDoc(status, None, None, status.text))),
      outputCodec = None,
    )

  /** Every error answers with `status`. For a sealed hierarchy with a status per case, use `outErrors`. */
  def outError[E1](status: Status)(using Schema[E1], JsonCodec[E1]): Endpoint[In, E1, Out] =
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
    replace(encodeOut, decodeOut, next, doc.copy(responses = responses), outputCodec)

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
      encodeOut,
      decodeOut,
      errors,
      outputCodec,
      doc,
      slots :+ Endpoint.InputSlot.mapped(from),
    )

  /** The request `decodeIn` reads back as `in`. Total: every builder records how to put its piece back. */
  def toRequest(in: In, base: Url): Request =
    val (pathIn, acc) = slots.foldRight[(Any, Endpoint.Acc)]((in, Endpoint.Acc.empty)) { case (slot, (cur, acc)) =>
      val (prev, piece) = slot.peel(cur)
      (prev, slot.put(piece, acc))
    }
    val encoded = path.encode(pathIn.asInstanceOf[PathIn])
    val url     = base.copy(path = Path(base.path.segments ++ encoded.segments), query = acc.query)
    Request(method, url, acc.headers, acc.body)

  def fromResponse(res: Response): IO[CallFailure[Err], Out] =
    if errors.handles(res.status) then decodeFailure(res)
    else if res.status.isSuccess then decodeOut(res).mapError(CallFailure.Undecodable(res.status, _))
    else ZIO.fail(CallFailure.Unexpected(res.status))

  private def decodeFailure(res: Response): IO[CallFailure[Err], Out] =
    Endpoint.bytes(res).mapError(CallFailure.Undecodable(res.status, _)).flatMap { raw =>
      errors.decode(res.status, raw) match
        case None            => ZIO.fail(CallFailure.Unexpected(res.status))
        case Some(Left(msg)) => ZIO.fail(CallFailure.Undecodable(res.status, msg))
        case Some(Right(e))  => ZIO.fail(CallFailure.Domain(e))
    }

  private def zipIn[P](in: In, piece: P): Combine[In, P] = Combine(in, piece)

  private def bindSync[N](
      next: (In, Request) => Either[Response, N],
      nextDoc: EndpointDoc,
      slot: Endpoint.InputSlot,
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
      decodeOut,
      errors,
      outputCodec,
      nextDoc,
      slots :+ slot,
    )

  private def bindNext[N](
      next: (In, Request) => IO[Response, N],
      nextDoc: EndpointDoc,
      slot: Endpoint.InputSlot,
  ): Endpoint[N, Err, Out] =
    Endpoint.Impl(
      method,
      path,
      (pathIn, req) => decodeIn(pathIn, req).flatMap(in => next(in, req)),
      encodeOut,
      decodeOut,
      errors,
      outputCodec,
      nextDoc,
      slots :+ slot,
    )

  private def replace[E1, O1](
      encodeOut: O1 => Response,
      decodeOut: Response => IO[String, O1],
      errors: ErrorCodec[E1],
      doc: EndpointDoc,
      outputCodec: Option[JsonCodec[O1]],
  ): Endpoint[In, E1, O1] =
    Endpoint.Impl(method, path, decodeIn, encodeOut, decodeOut, errors, outputCodec, doc, slots)

  private def replaceDoc(next: EndpointDoc): Endpoint[In, Err, Out] =
    replace(encodeOut, decodeOut, errors, next, outputCodec)

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
      Chunk.empty,
    )

  final class OutErrors[In, E, Out] private[endpoint] (endpoint: Endpoint[In, ?, Out]):
    inline def apply(inline cases: ErrorCase[? <: E]*)(using s: Schema[E], j: JsonCodec[E]): Endpoint[In, E, Out] =
      endpoint.withErrors(ErrorCaseMacros.codec[E](cases*)(using s, j))

  final case class Acc(query: QueryParams, headers: Headers, body: Body)

  object Acc:
    val empty: Acc = Acc(QueryParams.empty, Headers.empty, Body.empty)

  /** How one builder step splits its input back apart (`peel`) and writes its piece into the request (`put`). */
  final class InputSlot(val peel: Any => (Any, Any), val put: (Any, Acc) => Acc)

  object InputSlot:
    inline def combine[A, B](put: (Any, Acc) => Acc): InputSlot =
      InputSlot(
        n => Combine.unapply[A, B](n.asInstanceOf[Combine[A, B]]),
        put,
      )

    /** A `mapIn` step: writes nothing, and hands `from(b)` to the steps before it. */
    def mapped[A, B](from: B => A): InputSlot =
      InputSlot(b => (from(b.asInstanceOf[B]), ()), (_, acc) => acc)
  end InputSlot

  private final class Impl[P, In, Err, Out](
      val method: Method,
      val path: PathCodec[P],
      val decodeIn: (P, Request) => IO[Response, In],
      val encodeOut: Out => Response,
      val decodeOut: Response => IO[String, Out],
      val errors: ErrorCodec[Err],
      val outputCodec: Option[JsonCodec[Out]],
      val doc: EndpointDoc,
      val slots: Chunk[InputSlot],
  ) extends Endpoint[In, Err, Out]:
    type PathIn = P
  end Impl

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

  private def jsonBytes[A](a: A)(using c: JsonCodec[A]): Chunk[Byte] =
    Chunk.fromArray(c.encoder.encodeJson(a).toString.getBytes(StandardCharsets.UTF_8))

  private val unauthorized: StatusDoc = StatusDoc(Status.Unauthorized, None, None, "Unauthorized")

  private def jsonDecode[A](raw: Chunk[Byte])(using c: JsonCodec[A]): Either[String, A] =
    c.decoder.decodeJson(String(raw.toArray, StandardCharsets.UTF_8))

  private def bytes(res: Response): IO[String, Chunk[Byte]] =
    res.body.collect.mapError(e => s"unreadable body: ${e.getMessage}")
end Endpoint
