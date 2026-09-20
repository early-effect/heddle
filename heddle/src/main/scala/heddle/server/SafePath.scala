package heddle.server

import heddle.http.Path

/** Jail a user-controlled relative path under a root.
  *
  * `..`, `.`, empty segments, NUL, and backslash are rejected before any filesystem call. Percent-decoding is
  * [[Path.decode]]'s job: `%2e%2e` is already `..` in [[Path.segments]].
  */
object SafePath:
  def remainder(urlPrefix: String, path: Path): Option[List[String]] =
    val prefix = prefixSegments(urlPrefix)
    val segs   = path.segments.toList
    if !startsWith(segs, prefix) then None
    else
      val rest = segs.drop(prefix.length)
      if rest.exists(unsafeSegment) then None
      else Some(rest)

  def resolveUnder(root: String, relative: String): Option[String] =
    parseRelative(relative).flatMap(resolveUnder(root, _))

  def resolveUnder(root: String, segs: List[String]): Option[String] =
    if segs.exists(unsafeSegment) then None
    else if rootEscapes(root) then None
    else Some(join(root, segs))

  private def rootEscapes(root: String): Boolean =
    val n = root.replace('\\', '/').stripPrefix("/").stripSuffix("/")
    n.nonEmpty && (n.contains('\u0000') || n.split("/").exists(s => s.isEmpty || s == "." || s == ".."))

  private def parseRelative(relative: String): Option[List[String]] =
    if relative.isEmpty then Some(Nil)
    else if relative.contains('\u0000') then None
    else
      val n = relative.replace('\\', '/')
      if n.startsWith("/") then None
      else
        val parts = n.split("/", -1).toList
        if parts.exists(unsafeSegment) then None
        else Some(parts)

  private def unsafeSegment(s: String): Boolean =
    s.isEmpty || s == "." || s == ".." || s.contains('\u0000') || s.contains('/') || s.contains('\\')

  private def prefixSegments(urlPrefix: String): List[String] =
    val p = if urlPrefix.startsWith("/") then urlPrefix else s"/$urlPrefix"
    Path.decode(p).segments.toList

  private def startsWith(segs: List[String], prefix: List[String]): Boolean =
    segs.length >= prefix.length && segs.take(prefix.length) == prefix

  private def join(root: String, segs: List[String]): String =
    val r = root.replace('\\', '/').stripSuffix("/")
    if segs.isEmpty then r
    else if r.isEmpty then segs.mkString("/")
    else s"$r/${segs.mkString("/")}"
end SafePath
