package heddle.server

import heddle.http.{Body, MediaType, Request, Response, Status}
import heddle.http.header.{ByteRange, ContentRange, EntityTag, HeaderName}
import heddle.internal.node.{Buffers, Fs, NodePath}
import zio.*

object Files:
  def fromPath(path: String): Task[Response] =
    serveFile(path, None)

  def fromDirectory(
      root: String,
      urlPrefix: String,
      request: Request,
      indexHtml: Boolean,
  ): Task[Option[Response]] =
    ZIO
      .attempt {
        SafePath.remainder(urlPrefix, request.path).flatMap { rest =>
          SafePath.resolveUnder(root, rest).flatMap { joined =>
            val base = NodePath.resolve(root)
            val abs  = NodePath.resolve(joined)
            if !jailed(base, abs) then None
            else
              val st = Fs.statSync(abs)
              if st.isDirectory() then
                if indexHtml then
                  val index = NodePath.resolve(NodePath.join(abs, "index.html"))
                  if jailed(base, index) && Fs.statSync(index).isFile() then Some(index) else None
                else None
              else if st.isFile() then Some(abs)
              else None
            end if
          }
        }
      }
      .flatMap {
        case None    => ZIO.succeed(None)
        case Some(p) => serveFile(p, Some(request)).map(Some(_))
      }
      .catchAll(_ => ZIO.succeed(None))

  def fromResource(name: String, request: Request): Task[Option[Response]] =
    val _ = (name, request)
    ZIO.succeed(None)

  private def serveFile(path: String, request: Option[Request]): Task[Response] =
    ZIO
      .attempt {
        val st = Fs.statSync(path)
        if st.isDirectory() then throw java.io.IOException(s"is a directory: $path")
        if !st.isFile() then throw java.io.IOException(s"missing path: $path")
        val n     = st.size.toLong
        val mtime = java.time.Instant.ofEpochMilli(st.mtimeMs.toLong)
        val ct    = MediaType.fromExtension(extension(path))
        val etag  = EntityTag(s"$n-${mtime.toEpochMilli}", weak = true)
        val bytes = Buffers.fromU8(Fs.readFileSync(path))
        (n, mtime, ct, etag, bytes)
      }
      .map { (n, mtime, ct, etag, bytes) =>
        val base = Response(Status.Ok)
          .withHeader(HeaderName.ETag, etag.render)
          .withHeader(HeaderName.LastModified, heddle.http.header.HttpDate.render(mtime))
          .withHeader(HeaderName.AcceptRanges, "bytes")
        request match
          case Some(req) if notModified(req, etag, mtime) =>
            base.copy(status = Status.NotModified)
          case Some(req) =>
            req.headers.range.filter(_.unit.equalsIgnoreCase("bytes")).flatMap(_.ranges.headOption) match
              case Some(range) => ranged(base, bytes, n, ct, range)
              case None        => base.withBody(Body.fromBytes(bytes, Some(ct)))
          case None =>
            base.withBody(Body.fromBytes(bytes, Some(ct)))
      }

  private def notModified(req: Request, etag: EntityTag, mtime: java.time.Instant): Boolean =
    val inm = req.headers.ifNoneMatch
    if inm.nonEmpty then inm.exists(t => t.tag == etag.tag)
    else req.headers.ifModifiedSince.exists(t => !mtime.isAfter(t))

  private def ranged(
      base: Response,
      bytes: zio.Chunk[Byte],
      size: Long,
      ct: MediaType,
      range: ByteRange,
  ): Response =
    resolve(range, size) match
      case None =>
        base
          .copy(status = Status.RangeNotSatisfiable)
          .withHeader(HeaderName.ContentRange, ContentRange.unsatisfiable(size).render)
      case Some((start, end)) =>
        val slice = bytes.drop(start.toInt).take((end - start + 1).toInt)
        base
          .copy(status = Status.PartialContent)
          .withHeader(HeaderName.ContentRange, ContentRange.satisfiable("bytes", start, end, size).render)
          .withBody(Body.fromBytes(slice, Some(ct)))

  private def resolve(range: ByteRange, size: Long): Option[(Long, Long)] =
    range match
      case ByteRange.Inclusive(start, endOpt) =>
        if start >= size then None
        else
          val end = endOpt.map(e => math.min(e, size - 1)).getOrElse(size - 1)
          if end < start then None else Some((start, end))
      case ByteRange.Suffix(n) =>
        if size == 0 then None
        else Some((math.max(0L, size - n), size - 1))

  private def extension(path: String): String =
    val name = path.substring(path.lastIndexOf('/') + 1)
    val dot  = name.lastIndexOf('.')
    if dot < 0 then "" else name.substring(dot + 1)

  private def jailed(base: String, abs: String): Boolean =
    abs == base || abs.startsWith(base + "/") || abs.startsWith(base + "\\")
end Files
