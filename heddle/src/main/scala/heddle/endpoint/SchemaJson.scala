package heddle.endpoint

import scala.collection.mutable
import zio.Chunk
import zio.json.ast.Json

object SchemaJson:
  def render(doc: SchemaDoc, embedNamed: Boolean = true): Json =
    render(doc, mutable.LinkedHashMap.empty, embedNamed)

  private[heddle] def render(
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
        Json.Obj("type" -> Json.Str("array"), "items" -> render(items, components, embedNamed))
      case SchemaDoc.Object(title, _, _) if !embedNamed && title.isDefined =>
        Json.Obj("$ref" -> Json.Str(s"#/components/schemas/${title.get}"))
      case SchemaDoc.Object(title, fields, required) =>
        val props = Json.Obj(fields.map(f => f.name -> render(f.doc, components, embedNamed))*)
        val extra =
          title.map(t => "title" -> Json.Str(t)).toList ++
            List("type" -> Json.Str("object"), "properties" -> props) ++
            (if required.isEmpty then Nil
             else List("required" -> Json.Arr(Chunk.fromIterable(required.map(Json.Str(_)))))) ++
            (if fields.isEmpty then List("additionalProperties" -> Json.Bool(false)) else Nil)
        Json.Obj(extra*)
      case SchemaDoc.OneOf(title, variants) =>
        val oneOf = List("oneOf" -> Json.Arr(Chunk.fromIterable(variants.map(render(_, components, embedNamed)))))
        Json.Obj(title.map(t => "title" -> Json.Str(t)).toList ++ oneOf*)
      case SchemaDoc.Optional(inner) =>
        render(inner, components, embedNamed)
      case SchemaDoc.Ref(name) =>
        Json.Obj("$ref" -> Json.Str(s"#/components/schemas/$name"))

  private def typed(tpe: String, format: Option[String]): Json =
    Json.Obj(List("type" -> Json.Str(tpe)) ++ format.map(f => "format" -> Json.Str(f)).toList*)
end SchemaJson
