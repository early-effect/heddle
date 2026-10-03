package heddle.endpoint

import heddle.http.{Body, QueryParams, Request, Url, UrlEncoding}
import heddle.http.header.Headers
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

object OpArgs:
  def promotable(doc: EndpointDoc): Boolean =
    val bodyOk = doc.requestBody.forall(_.contentType.isJson)
    val outOk  = doc.responses.exists(r => r.status.isSuccess && r.schema.isDefined && r.contentType.exists(_.isJson))
    bodyOk && outOk

  def inputSchema(doc: EndpointDoc): Either[OpArgsError, SchemaDoc] =
    val pathFields  = doc.pathParams.map(p => SchemaField(p.name, p.schema, optional = !p.required))
    val queryFields = doc.queries.map(p => SchemaField(p.name, p.schema, optional = !p.required))
    val taken       = (doc.pathParams.map(_.name) ++ doc.queries.map(_.name)).toSet
    bodyFields(doc, taken).map { body =>
      val fields   = pathFields ++ queryFields ++ body
      val required = fields.filterNot(_.optional).map(_.name)
      SchemaDoc.Object(None, fields, required)
    }

  def request(doc: EndpointDoc, args: Json, headers: Headers = Headers.empty): Either[OpArgsError, Request] =
    args match
      case obj: Json.Obj => requestObj(doc, obj, headers)
      case _             => Left(OpArgsError.NotAnObject)

  private def bodyFields(doc: EndpointDoc, taken: Set[String]): Either[OpArgsError, List[SchemaField]] =
    doc.requestBody match
      case None                                   => Right(Nil)
      case Some(body) if !body.contentType.isJson =>
        Left(OpArgsError.NonJsonBody)
      case Some(body) =>
        val (inner, optional) = body.schema.unwrapOptional
        if doc.nestBody then Right(List(SchemaField(bodyName(inner), inner, optional)))
        else
          inner match
            case SchemaDoc.Object(_, fields, _) =>
              val clash = fields.map(_.name).filter(taken.contains)
              if clash.nonEmpty then Left(OpArgsError.NameCollision(clash))
              else Right(fields)
            case other =>
              Right(List(SchemaField(bodyName(other), other, optional)))

  private def bodyName(schema: SchemaDoc): String =
    schema match
      case SchemaDoc.Object(Some(title), _, _) => title
      case SchemaDoc.OneOf(Some(title), _)     => title
      case SchemaDoc.Enum(Some(title), _)      => title
      case _                                   => "body"

  private def requestObj(doc: EndpointDoc, args: Json.Obj, headers: Headers): Either[OpArgsError, Request] =
    for
      path  <- fillPath(doc, args)
      query <- fillQuery(doc, args)
      body  <- fillBody(doc, args)
    yield
      val url = Url(heddle.http.Path.decode(path), query)
      Request(doc.method, url, headers, body)

  private def fillPath(doc: EndpointDoc, args: Json.Obj): Either[OpArgsError, String] =
    doc.pathParams.foldLeft[Either[OpArgsError, String]](Right(doc.pathTemplate)):
      case (Left(err), _)  => Left(err)
      case (Right(tpl), p) =>
        args.get(p.name) match
          case None if p.required => Left(OpArgsError.Missing(ParamLocation.Path, p.name))
          case None               => Right(tpl)
          case Some(json)         =>
            atom(json) match
              case None    => Left(OpArgsError.NotScalar(ParamLocation.Path, p.name))
              case Some(v) => Right(tpl.replace(s"{${p.name}}", UrlEncoding.encode(v)))

  private def fillQuery(doc: EndpointDoc, args: Json.Obj): Either[OpArgsError, QueryParams] =
    doc.queries
      .foldLeft[Either[OpArgsError, List[(String, String)]]](Right(Nil)):
        case (Left(err), _)  => Left(err)
        case (Right(acc), p) =>
          args.get(p.name) match
            case None if p.required => Left(OpArgsError.Missing(ParamLocation.Query, p.name))
            case None               => Right(acc)
            case Some(json)         =>
              atom(json) match
                case None    => Left(OpArgsError.NotScalar(ParamLocation.Query, p.name))
                case Some(v) => Right(acc :+ (p.name -> v))
      .map(pairs => QueryParams.of(pairs*))

  private def fillBody(doc: EndpointDoc, args: Json.Obj): Either[OpArgsError, Body] =
    doc.requestBody match
      case None                                     => Right(Body.empty)
      case Some(media) if !media.contentType.isJson =>
        Left(OpArgsError.NonJsonBody)
      case Some(media) =>
        val taken      = (doc.pathParams.map(_.name) ++ doc.queries.map(_.name)).toSet
        val (inner, _) = media.schema.unwrapOptional
        val payload    =
          if doc.nestBody then args.get(bodyName(inner)).orElse(args.get("body"))
          else
            inner match
              case SchemaDoc.Object(_, fields, _) =>
                val keys = fields.map(_.name).toSet
                Some(Json.Obj(Chunk.fromIterable(args.fields.filter((k, _) => keys.contains(k) && !taken.contains(k)))))
              case _ =>
                args.get(bodyName(inner)).orElse(args.get("body"))
        payload match
          case None    => Left(OpArgsError.MissingBody)
          case Some(j) => Right(Body.json(j.toJson))

  private def atom(json: Json): Option[String] =
    json match
      case Json.Str(s)  => Some(s)
      case Json.Num(n)  => Some(n.stripTrailingZeros.toPlainString)
      case Json.Bool(b) => Some(b.toString)
      case Json.Null    => None
      case _            => None

  /** The arguments `request` turns back into `req`. Headers never travel in arguments, so an endpoint whose input reads
    * a header has no argument form for that piece.
    */
  def arguments(doc: EndpointDoc, req: Request): Either[OpArgsError, Json.Obj] =
    for
      path  <- pathArgs(doc, req)
      query <- queryArgs(doc, req)
      body  <- bodyArgs(doc, req)
    yield Json.Obj(Chunk.fromIterable(path ++ query ++ body))

  private def pathArgs(doc: EndpointDoc, req: Request): Either[OpArgsError, List[(String, Json)]] =
    val template = doc.pathTemplate.split('/').toList.filter(_.nonEmpty)
    val actual   = req.path.segments.toList
    if template.length != actual.length then Left(OpArgsError.PathMismatch(req.path.render, doc.pathTemplate))
    else
      val byName = doc.pathParams.map(p => p.name -> p.schema).toMap
      Right(template.zip(actual).collect {
        case (seg, value) if seg.startsWith("{") && seg.endsWith("}") =>
          val name = seg.drop(1).dropRight(1)
          name -> scalar(byName.getOrElse(name, SchemaDoc.Str(None)), value)
      })
  end pathArgs

  private def queryArgs(doc: EndpointDoc, req: Request): Either[OpArgsError, List[(String, Json)]] =
    doc.queries.foldLeft[Either[OpArgsError, List[(String, Json)]]](Right(Nil)) {
      case (Left(err), _)  => Left(err)
      case (Right(acc), p) =>
        req.query.getAll(p.name).toList match
          case Nil          => Right(acc)
          case value :: Nil => Right(acc :+ (p.name -> scalar(p.schema.unwrapOptional._1, value)))
          case _            => Left(OpArgsError.RepeatedQuery(p.name))
    }

  private def bodyArgs(doc: EndpointDoc, req: Request): Either[OpArgsError, List[(String, Json)]] =
    doc.requestBody match
      case None                                     => Right(Nil)
      case Some(media) if !media.contentType.isJson =>
        Left(OpArgsError.NonJsonBody)
      case Some(media) =>
        val (inner, _) = media.schema.unwrapOptional
        req.body.text match
          case None      => Left(OpArgsError.StreamedBody)
          case Some(raw) =>
            raw.fromJson[Json].left.map(OpArgsError.BodyNotJson(_)).map { json =>
              (doc.nestBody, inner, json) match
                case (false, SchemaDoc.Object(_, _, _), obj: Json.Obj) => obj.fields.toList
                case _                                                 => List(bodyName(inner) -> json)
            }

  private def scalar(schema: SchemaDoc, value: String): Json =
    schema match
      case SchemaDoc.Integer(_) | SchemaDoc.Number(_) =>
        scala.util.Try(java.math.BigDecimal(value)).toOption.fold[Json](Json.Str(value))(Json.Num(_))
      case SchemaDoc.Boolean =>
        value.toBooleanOption.fold[Json](Json.Str(value))(Json.Bool(_))
      case _ => Json.Str(value)
end OpArgs
