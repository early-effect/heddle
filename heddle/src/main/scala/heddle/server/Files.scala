package heddle.server

import heddle.http.{Body, MediaType, Request, Response, Status}
import heddle.http.header.{ByteRange, ContentRange, EntityTag, HeaderName}
import java.nio.file.{Files as JFiles, Path}
import zio.*
import zio.stream.ZStream

object Files:
  def fromPath(path: Path): Task[Response] =
    serveFile(path, None)

  def fromPath(path: Path, request: Request): Task[Response] =
    serveFile(path, Some(request))

  def fromDirectory(
      root: Path,
      urlPrefix: String,
      request: Request,
      indexHtml: Boolean = true,
  ): Task[Option[Response]] =
    ZIO
      .attemptBlocking {
        val prefix = normalizePrefix(urlPrefix)
        val rel    = stripPrefix(request.path.render, prefix)
        rel.map { rest =>
          val base = root.toAbsolutePath.normalize()
          val raw  = if rest.isEmpty || rest == "/" then base else base.resolve(rest.stripPrefix("/")).normalize()
          if !raw.startsWith(base) then None
          else if JFiles.isDirectory(raw) then
            if indexHtml then
              val index = raw.resolve("index.html").normalize()
              if index.startsWith(base) && JFiles.isRegularFile(index) then Some(index) else None
            else None
          else if JFiles.isRegularFile(raw) then Some(raw)
          else None
        }
      }
      .flatMap {
        case None          => ZIO.succeed(None)
        case Some(None)    => ZIO.succeed(None)
        case Some(Some(p)) => serveFile(p, Some(request)).map(Some(_))
      }

  def fromResource(name: String, request: Request): Task[Option[Response]] =
    ZIO
      .attemptBlocking {
        val cleaned = name.stripPrefix("/").replace('\\', '/')
        if cleaned.contains("..") then None
        else Option(Thread.currentThread().getContextClassLoader.getResource(cleaned))
      }
      .flatMap {
        case None      => ZIO.succeed(None)
        case Some(url) =>
          url.getProtocol match
            case "file" => serveFile(Path.of(url.toURI), Some(request)).map(Some(_))
            case _      =>
              ZIO
                .attemptBlocking {
                  val in = url.openStream()
                  try in.readAllBytes()
                  finally in.close()
                }
                .map { bytes =>
                  val ct = MediaType.fromExtension(extension(Path.of(name)))
                  Some(
                    Response(Status.Ok)
                      .withBody(Body.fromBytes(zio.Chunk.fromArray(bytes), Some(ct)))
                      .withHeader(HeaderName.ContentLength, bytes.length.toString)
                  )
                }
      }

  private def serveFile(path: Path, request: Option[Request]): Task[Response] =
    ZIO
      .attemptBlocking {
        val p = path.toAbsolutePath.normalize()
        if !JFiles.exists(p) then throw java.nio.file.NoSuchFileException(p.toString)
        if JFiles.isDirectory(p) then throw java.nio.file.AccessDeniedException(p.toString, null, "is a directory")
        val n     = JFiles.size(p)
        val mtime = JFiles.getLastModifiedTime(p).toInstant
        val ct    = MediaType.fromExtension(extension(p))
        val etag  = EntityTag(s"$n-${mtime.toEpochMilli}", weak = true)
        (p, n, mtime, ct, etag)
      }
      .map { (p, n, mtime, ct, etag) =>
        val base = Response(Status.Ok)
          .withHeader(HeaderName.ETag, etag.render)
          .withHeader(HeaderName.LastModified, heddle.http.header.HttpDate.render(mtime))
          .withHeader(HeaderName.AcceptRanges, "bytes")
        request match
          case Some(req) if notModified(req, etag, mtime) =>
            base.copy(status = Status.NotModified)
          case Some(req) =>
            req.headers.range.filter(_.unit.equalsIgnoreCase("bytes")).flatMap(_.ranges.headOption) match
              case Some(range) => ranged(base, p, n, ct, range)
              case None        => base.withBody(Body.stream(ZStream.fromPath(p), Some(ct), Some(n)))
          case None =>
            base.withBody(Body.stream(ZStream.fromPath(p), Some(ct), Some(n)))
      }

  private def notModified(req: Request, etag: EntityTag, mtime: java.time.Instant): Boolean =
    val inm = req.headers.ifNoneMatch
    if inm.nonEmpty then inm.exists(t => t.tag == etag.tag)
    else req.headers.ifModifiedSince.exists(t => !mtime.isAfter(t))

  private def ranged(base: Response, path: Path, size: Long, ct: MediaType, range: ByteRange): Response =
    resolve(range, size) match
      case None =>
        base
          .copy(status = Status.RangeNotSatisfiable)
          .withHeader(HeaderName.ContentRange, ContentRange.unsatisfiable(size).render)
      case Some((start, end)) =>
        val len = end - start + 1
        base
          .copy(status = Status.PartialContent)
          .withHeader(HeaderName.ContentRange, ContentRange.satisfiable("bytes", start, end, size).render)
          .withBody(Body.stream(sliceStream(path, start, len), Some(ct), Some(len)))

  private def resolve(range: ByteRange, size: Long): Option[(Long, Long)] =
    range match
      case ByteRange.Inclusive(start, endOpt) =>
        if start >= size then None
        else
          val end = endOpt.map(e => math.min(e, size - 1)).getOrElse(size - 1)
          if end < start then None else Some((start, end))
      case ByteRange.Suffix(n) =>
        if size == 0 then None
        else
          val start = math.max(0L, size - n)
          Some((start, size - 1))

  private def sliceStream(path: Path, start: Long, len: Long): ZStream[Any, Throwable, Byte] =
    ZStream
      .fromInputStreamZIO(
        ZIO
          .attemptBlocking {
            val in   = JFiles.newInputStream(path)
            var left = start
            while left > 0 do
              val skipped = in.skip(left)
              if skipped <= 0 then left = 0
              else left -= skipped
            in
          }
          .refineToOrDie[java.io.IOException]
      )
      .take(len)

  private def normalizePrefix(prefix: String): String =
    val p = if prefix.startsWith("/") then prefix else s"/$prefix"
    if p.endsWith("/") then p.dropRight(1) else p

  private def stripPrefix(path: String, prefix: String): Option[String] =
    if path == prefix then Some("")
    else if path.startsWith(prefix + "/") then Some(path.substring(prefix.length))
    else None

  private def extension(path: Path): String =
    val name = path.getFileName.toString
    val dot  = name.lastIndexOf('.')
    if dot < 0 then "" else name.substring(dot + 1)
end Files
