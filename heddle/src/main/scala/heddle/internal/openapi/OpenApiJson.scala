package heddle.internal.openapi

import heddle.endpoint.{EndpointDoc, OpenApi, SchemaDoc, StatusDoc}
import scala.collection.mutable
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

private[heddle] object OpenApiJson:
  def render(spec: OpenApi): String =
    val components = mutable.LinkedHashMap.empty[String, SchemaDoc]
    val ops        = spec.endpoints.map { ep =>
      val rewritten = rewriteEndpoint(ep, components)
      rewritten.pathTemplate -> rewritten
    }
    val paths = ops
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .map { (path, items) =>
        val methods = items.map { (_, ep) =>
          ep.method.render.toLowerCase -> operationJson(ep)
        }
        path -> Json.Obj(methods*)
      }
    val schemaFields =
      components.toList.map((name, doc) => name -> schemaJson(doc, components, embedNamed = true))
    val infoFields =
      List("title" -> Json.Str(spec.title), "version" -> Json.Str(spec.version)) ++
        spec.description.map(d => "description" -> Json.Str(d)).toList
    val root =
      List(
        "openapi" -> Json.Str("3.1.0"),
        "info"    -> Json.Obj(infoFields*),
        "paths"   -> Json.Obj(paths*),
      ) ++ (
        if schemaFields.isEmpty then Nil
        else List("components" -> Json.Obj("schemas" -> Json.Obj(schemaFields*)))
      )
    Json.Obj(root*).toJson
  end render

  private def rewriteEndpoint(ep: EndpointDoc, components: mutable.LinkedHashMap[String, SchemaDoc]): EndpointDoc =
    ep.copy(
      pathParams = ep.pathParams.map(p => p.copy(schema = register(p.schema, components))),
      queries = ep.queries.map(p => p.copy(schema = register(p.schema, components))),
      headers = ep.headers.map(p => p.copy(schema = register(p.schema, components))),
      requestBody = ep.requestBody.map(b => b.copy(schema = register(b.schema, components))),
      responses = ep.responses.map(r => r.copy(schema = r.schema.map(register(_, components)))),
    )

  private def register(doc: SchemaDoc, components: mutable.LinkedHashMap[String, SchemaDoc]): SchemaDoc =
    doc match
      case _ @SchemaDoc.Object(Some(title), fields, required) =>
        val rewritten =
          SchemaDoc.Object(Some(title), fields.map(f => f.copy(doc = register(f.doc, components))), required)
        components.getOrElseUpdate(title, rewritten)
        SchemaDoc.Ref(title)
      case _ @SchemaDoc.OneOf(Some(title), variants) =>
        val rewritten = SchemaDoc.OneOf(Some(title), variants.map(register(_, components)))
        components.getOrElseUpdate(title, rewritten)
        SchemaDoc.Ref(title)
      case SchemaDoc.OneOf(None, variants) =>
        SchemaDoc.OneOf(None, variants.map(register(_, components)))
      case SchemaDoc.Object(None, fields, required) =>
        SchemaDoc.Object(None, fields.map(f => f.copy(doc = register(f.doc, components))), required)
      case SchemaDoc.Array(items) =>
        SchemaDoc.Array(register(items, components))
      case SchemaDoc.Optional(inner) =>
        register(inner, components)
      case other => other

  private def operationJson(ep: EndpointDoc): Json =
    val params =
      (ep.pathParams ++ ep.queries ++ ep.headers).map { p =>
        Json.Obj(
          "name"     -> Json.Str(p.name),
          "in"       -> Json.Str(p.in),
          "required" -> Json.Bool(p.required),
          "schema"   -> schemaJson(p.schema, mutable.LinkedHashMap.empty, embedNamed = true),
        )
      }
    val requestBody = ep.requestBody.map { body =>
      "requestBody" -> Json.Obj(
        "required" -> Json.Bool(true),
        "content"  -> Json.Obj(
          body.contentType.render -> Json.Obj(
            "schema" -> schemaJson(body.schema, mutable.LinkedHashMap.empty, embedNamed = true)
          )
        ),
      )
    }
    val responses = ep.responses.map { r =>
      r.status.code.toString -> responseJson(r)
    }
    val fields =
      ep.operationId.map(id => "operationId" -> Json.Str(id)).toList ++
        ep.summary.map(s => "summary" -> Json.Str(s)).toList ++
        ep.description.map(d => "description" -> Json.Str(d)).toList ++
        (if ep.tags.isEmpty then Nil else List("tags" -> Json.Arr(Chunk.fromIterable(ep.tags.map(Json.Str(_)))))) ++
        (if params.isEmpty then Nil else List("parameters" -> Json.Arr(Chunk.fromIterable(params)))) ++
        requestBody.toList ++
        List("responses" -> Json.Obj(responses*))
    Json.Obj(fields*)
  end operationJson

  private def responseJson(r: StatusDoc): Json =
    val content = (r.schema, r.contentType) match
      case (Some(schema), Some(ct)) =>
        List(
          "content" -> Json.Obj(
            ct.render -> Json.Obj(
              "schema" -> schemaJson(schema, mutable.LinkedHashMap.empty, embedNamed = true)
            )
          )
        )
      case _ => Nil
    Json.Obj(("description" -> Json.Str(r.description)) :: content*)

  private def schemaJson(
      doc: SchemaDoc,
      components: mutable.LinkedHashMap[String, SchemaDoc],
      embedNamed: Boolean,
  ): Json =
    doc match
      case SchemaDoc.Null =>
        Json.Obj("type" -> Json.Str("null"))
      case SchemaDoc.Boolean =>
        Json.Obj("type" -> Json.Str("boolean"))
      case SchemaDoc.Integer(format) =>
        typed("integer", format)
      case SchemaDoc.Number(format) =>
        typed("number", format)
      case SchemaDoc.Str(format) =>
        typed("string", format)
      case SchemaDoc.Array(items) =>
        Json.Obj("type" -> Json.Str("array"), "items" -> schemaJson(items, components, embedNamed))
      case SchemaDoc.Object(title, _, _) if !embedNamed && title.isDefined =>
        Json.Obj("$ref" -> Json.Str(s"#/components/schemas/${title.get}"))
      case SchemaDoc.Object(title, fields, required) =>
        val props   = Json.Obj(fields.map(f => f.name -> schemaJson(f.doc, components, embedNamed))*)
        val fields0 =
          title.map(t => "title" -> Json.Str(t)).toList ++
            List("type" -> Json.Str("object"), "properties" -> props) ++
            (if required.isEmpty then Nil
             else List("required" -> Json.Arr(Chunk.fromIterable(required.map(Json.Str(_))))))
        Json.Obj(fields0*)
      case SchemaDoc.OneOf(title, variants) =>
        val oneOf = List("oneOf" -> Json.Arr(Chunk.fromIterable(variants.map(schemaJson(_, components, embedNamed)))))
        Json.Obj(title.map(t => "title" -> Json.Str(t)).toList ++ oneOf*)
      case SchemaDoc.Optional(inner) =>
        schemaJson(inner, components, embedNamed)
      case SchemaDoc.Ref(name) =>
        Json.Obj("$ref" -> Json.Str(s"#/components/schemas/$name"))

  private def typed(tpe: String, format: Option[String]): Json =
    Json.Obj(List("type" -> Json.Str(tpe)) ++ format.map(f => "format" -> Json.Str(f)).toList*)
end OpenApiJson
