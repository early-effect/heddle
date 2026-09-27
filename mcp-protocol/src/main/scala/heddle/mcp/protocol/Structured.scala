package heddle.mcp.protocol

import zio.json.ast.Json

/** How a tool's typed output travels as `structuredContent`, which MCP requires to be an object with an object
  * `outputSchema`. Chosen once from the output's JSON Schema: an object schema goes as is; anything else (a string, a
  * list, a `oneOf` of cases) goes as `{"value": v}` with the schema wrapped to match.
  */
enum Structured:
  case Direct(schema: Json.Obj)
  case Wrapped(inner: Json)

  def outputSchema: Json.Obj =
    this match
      case Direct(s)  => s
      case Wrapped(s) =>
        Json.Obj(
          "type"       -> Json.Str("object"),
          "properties" -> Json.Obj(Structured.Field -> s),
          "required"   -> Json.Arr(Json.Str(Structured.Field)),
        )

  /** Total for a value that conforms to the schema this shape came from. */
  def wrap(value: Json): Json.Obj =
    (this, value) match
      case (Direct(_), o: Json.Obj) => o
      case _                        => Json.Obj(Structured.Field -> value)

  def unwrap(structured: Json.Obj): Json =
    this match
      case Direct(_)  => structured
      case Wrapped(_) => structured.get(Structured.Field).getOrElse(Json.Null)
end Structured

object Structured:
  val Field = "value"

  def of(schema: Json): Structured =
    schema match
      case o: Json.Obj if o.get("type").contains(Json.Str("object")) => Direct(o)
      case other                                                     => Wrapped(other)
end Structured
