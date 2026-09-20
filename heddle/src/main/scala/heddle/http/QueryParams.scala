package heddle.http

import zio.Chunk

final case class QueryParams(toMap: Map[String, Chunk[String]]):
  def get(name: String): Option[String] = toMap.get(name).flatMap(_.headOption)

  def getAll(name: String): Chunk[String] = toMap.getOrElse(name, Chunk.empty)

  def isEmpty: Boolean = toMap.isEmpty || toMap.values.forall(_.isEmpty)

  def render: String =
    toMap.toList
      .sortBy(_._1)
      .flatMap { (k, vs) =>
        vs.map(v => s"${Path.percentEncode(k)}=${Path.percentEncode(v)}")
      }
      .mkString("&")
end QueryParams

object QueryParams:
  val empty: QueryParams = QueryParams(Map.empty)

  def of(pairs: (String, String)*): QueryParams =
    val grouped = pairs.groupMap(_._1)(_._2).view.mapValues(vs => Chunk.fromIterable(vs)).toMap
    QueryParams(grouped)

  def decode(raw: String): QueryParams =
    if raw.isEmpty then empty
    else
      val pairs = raw.split("&", -1).toList.flatMap { part =>
        if part.isEmpty then None
        else
          val eq     = part.indexOf('=')
          val (k, v) =
            if eq < 0 then (decodeComponent(part), "")
            else (decodeComponent(part.substring(0, eq)), decodeComponent(part.substring(eq + 1)))
          Some(k -> v)
      }
      of(pairs*)

  private def decodeComponent(s: String): String = UrlEncoding.decodeForm(s)
end QueryParams
