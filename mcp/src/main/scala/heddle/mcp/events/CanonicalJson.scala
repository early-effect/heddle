package heddle.mcp.events

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** Object keys sorted, array order kept, rendered by zio-json. Equality is this string. */
private[events] object CanonicalJson:
  def apply(json: Json): String = sorted(json).toJson

  private def sorted(json: Json): Json =
    json match
      case Json.Obj(fields) =>
        val values = fields.foldLeft(Map.empty[String, Json]) { case (acc, (key, value)) =>
          acc.updated(key, sorted(value))
        }
        val keys = values.keys.toList.sorted
        Json.Obj(Chunk.fromIterable(keys.flatMap(key => values.get(key).map(key -> _))))
      case Json.Arr(items) => Json.Arr(items.map(sorted))
      case other           => other
end CanonicalJson
