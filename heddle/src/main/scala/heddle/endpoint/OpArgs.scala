package heddle.endpoint

import heddle.http.{Body, MediaType, QueryParams, Request, Url}
import heddle.http.header.Headers
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

object OpArgs:
  def promotable(doc: EndpointDoc): Boolean =
    val bodyOk = doc.requestBody.forall(_.contentType == MediaType.Json)
    val outOk  = doc.responses.exists(r =>
      r.status.code >= 200 && r.status.code < 300 && r.schema.isDefined &&
        r.contentType.contains(MediaType.Json)
    )
    bodyOk && outOk

  def inputSchema(doc: EndpointDoc): Either[String, SchemaDoc] =
    val pathFields  = doc.pathParams.map(p => SchemaField(p.name, p.schema, optional = !p.required))
    val queryFields = doc.queries.map(p => SchemaField(p.name, p.schema, optional = !p.required))
    val taken       = (doc.pathParams.map(_.name) ++ doc.queries.map(_.name)).toSet
    bodyFields(doc, taken).map { body =>
      val fields   = pathFields ++ queryFields ++ body
      val required = fields.filterNot(_.optional).map(_.name)
      SchemaDoc.Object(None, fields, required)
    }

  def request(doc: EndpointDoc, args: Json, headers: Headers = Headers.empty): Either[String, Request] =
    args match
      case obj: Json.Obj => requestObj(doc, obj, headers)
      case _             => Left("arguments must be a JSON object")

  private def bodyFields(doc: EndpointDoc, taken: Set[String]): Either[String, List[SchemaField]] =
    doc.requestBody match
      case None                                             => Right(Nil)
      case Some(body) if body.contentType != MediaType.Json =>
        Left(s"${doc.toolName}: non-JSON body cannot be flattened")
      case Some(body) =>
        val (inner, optional) = body.schema.unwrapOptional
        if doc.nestBody then Right(List(SchemaField(bodyName(inner), inner, optional)))
        else
          inner match
            case SchemaDoc.Object(_, fields, _) =>
              val clash = fields.map(_.name).filter(taken.contains)
              if clash.nonEmpty then Left(s"${doc.toolName}: argument name collision: ${clash.mkString(", ")}")
              else Right(fields)
            case other =>
              Right(List(SchemaField(bodyName(other), other, optional)))

  private def bodyName(schema: SchemaDoc): String =
    schema match
      case SchemaDoc.Object(Some(title), _, _) => title
      case SchemaDoc.OneOf(Some(title), _)     => title
      case _                                   => "body"

  private def requestObj(doc: EndpointDoc, args: Json.Obj, headers: Headers): Either[String, Request] =
    for
      path  <- fillPath(doc, args)
      query <- fillQuery(doc, args)
      body  <- fillBody(doc, args)
    yield
      val url = Url(heddle.http.Path.decode(path), query)
      Request(doc.method, url, headers, body)

  private def fillPath(doc: EndpointDoc, args: Json.Obj): Either[String, String] =
    doc.pathParams.foldLeft[Either[String, String]](Right(doc.pathTemplate)):
      case (Left(err), _)  => Left(err)
      case (Right(tpl), p) =>
        args.get(p.name) match
          case None if p.required => Left(s"missing path argument '${p.name}'")
          case None               => Right(tpl)
          case Some(json)         =>
            atom(json) match
              case None    => Left(s"path argument '${p.name}' must be a scalar")
              case Some(v) => Right(tpl.replace(s"{${p.name}}", v))

  private def fillQuery(doc: EndpointDoc, args: Json.Obj): Either[String, QueryParams] =
    doc.queries
      .foldLeft[Either[String, List[(String, String)]]](Right(Nil)):
        case (Left(err), _)  => Left(err)
        case (Right(acc), p) =>
          args.get(p.name) match
            case None if p.required => Left(s"missing query argument '${p.name}'")
            case None               => Right(acc)
            case Some(json)         =>
              atom(json) match
                case None    => Left(s"query argument '${p.name}' must be a scalar")
                case Some(v) => Right(acc :+ (p.name -> v))
      .map(pairs => QueryParams.of(pairs*))

  private def fillBody(doc: EndpointDoc, args: Json.Obj): Either[String, Body] =
    doc.requestBody match
      case None                                               => Right(Body.empty)
      case Some(media) if media.contentType != MediaType.Json =>
        Left(s"${doc.toolName}: non-JSON body cannot be built from arguments")
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
          case None    => Left(s"${doc.toolName}: missing body arguments")
          case Some(j) => Right(Body.json(j.toJson))

  private def atom(json: Json): Option[String] =
    json match
      case Json.Str(s)  => Some(s)
      case Json.Num(n)  => Some(n.toString)
      case Json.Bool(b) => Some(b.toString)
      case Json.Null    => None
      case _            => None
end OpArgs
