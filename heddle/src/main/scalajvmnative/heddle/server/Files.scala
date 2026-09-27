package heddle.server

import heddle.error.FileError
import heddle.http.{Body, MediaType, Request, Response, Status}
import heddle.http.header.{ByteRange, ContentRange, EntityTag, HeaderName}
import java.nio.channels.FileChannel
import java.nio.file.{Files as JFiles, Path, StandardOpenOption}
import zio.*
import zio.stream.ZStream

object Files:
  def fromPath(path: String): IO[FileError, Response] =
    ZIO.attempt(Path.of(path)).mapError(FileError.Unreadable(path, _)).flatMap(fromPath)

  def fromPath(path: Path): IO[FileError, Response] =
    serveFile(path, None)

  def fromPath(path: Path, request: Request): IO[FileError, Response] =
    serveFile(path, Some(request))

  def fromDirectory(
      root: String,
      urlPrefix: String,
      request: Request,
      indexHtml: Boolean,
  ): IO[FileError, Option[Response]] =
    ZIO
      .attempt(Path.of(root))
      .mapError(FileError.Unreadable(root, _))
      .flatMap(fromDirectory(_, urlPrefix, request, indexHtml))

  def fromDirectory(
      root: Path,
      urlPrefix: String,
      request: Request,
      indexHtml: Boolean,
  ): IO[FileError, Option[Response]] =
    ZIO
      .attemptBlocking {
        SafePath.remainder(urlPrefix, request.path).map { rest =>
          val base = root.toAbsolutePath.normalize()
          val raw  = rest.foldLeft(base)((p, s) => p.resolve(s)).normalize()
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
      .mapError(FileError.Unreadable(root.toString, _))
      .flatMap {
        case None          => ZIO.succeed(None)
        case Some(None)    => ZIO.succeed(None)
        case Some(Some(p)) => serveFile(p, Some(request)).map(Some(_))
      }

  def fromResource(name: String, request: Request): IO[FileError, Option[Response]] =
    ZIO
      .attemptBlocking {
        SafePath.resolveUnder("", name.stripPrefix("/")) match
          case None        => None
          case Some(clean) =>
            if clean.contains('\u0000') then None
            else Option(Thread.currentThread().getContextClassLoader.getResource(clean))
      }
      .mapError(FileError.Unreadable(name, _))
      .flatMap {
        case None      => ZIO.succeed(None)
        case Some(url) =>
          url.getProtocol match
            case "file" =>
              ZIO
                .attempt(Path.of(url.toURI))
                .mapError(FileError.Unreadable(name, _))
                .flatMap(serveFile(_, Some(request)))
                .map(Some(_))
            case _ =>
              ZIO
                .scoped(
                  ZIO
                    .fromAutoCloseable(ZIO.attemptBlocking(url.openStream()))
                    .flatMap(in => ZIO.attemptBlocking(in.readAllBytes()))
                )
                .mapError(FileError.Unreadable(name, _))
                .map { bytes =>
                  val ct = MediaType.fromExtension(extension(Path.of(name)))
                  Some(
                    Response(Status.Ok)
                      .withBody(Body.fromBytes(zio.Chunk.fromArray(bytes), Some(ct)))
                      .withHeader(HeaderName.ContentLength, bytes.length.toString)
                  )
                }
      }

  private final case class Found(path: Path, size: Long, mtime: java.time.Instant, ct: MediaType, etag: EntityTag)

  private def stat(path: Path): IO[FileError, Found] =
    ZIO
      .attemptBlocking {
        val p = path.toAbsolutePath.normalize()
        if !JFiles.exists(p) then Left(FileError.NotFound(p.toString))
        else if JFiles.isDirectory(p) then Left(FileError.IsDirectory(p.toString))
        else
          val n     = JFiles.size(p)
          val mtime = JFiles.getLastModifiedTime(p).toInstant
          Right(
            Found(
              p,
              n,
              mtime,
              MediaType.fromExtension(extension(p)),
              EntityTag(s"$n-${mtime.toEpochMilli}", weak = true),
            )
          )
        end if
      }
      .mapError(FileError.Unreadable(path.toString, _))
      .flatMap(ZIO.fromEither(_))

  private def serveFile(path: Path, request: Option[Request]): IO[FileError, Response] =
    stat(path)
      .map { case Found(p, n, mtime, ct, etag) =>
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
    ZStream.unwrapScoped {
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking {
              val ch = FileChannel.open(path, StandardOpenOption.READ)
              if start > 0 then ch.position(start)
              java.nio.channels.Channels.newInputStream(ch)
            }
            .refineToOrDie[java.io.IOException]
        )(in => ZIO.attempt(in.close()).ignore)
        .map(in => ZStream.fromInputStream(in, 8192).take(len))
    }

  private def extension(path: Path): String =
    val name = path.getFileName.toString
    val dot  = name.lastIndexOf('.')
    if dot < 0 then "" else name.substring(dot + 1)
end Files
