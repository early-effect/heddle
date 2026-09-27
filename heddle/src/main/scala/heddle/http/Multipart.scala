package heddle.http

import java.nio.charset.StandardCharsets
import java.util.Arrays
import zio.{Chunk, Ref, UIO, ZIO}
import zio.stream.ZStream

enum FormField:
  case Text(name: String, value: String, contentType: Option[MediaType] = None)
  case Binary(
      name: String,
      data: Chunk[Byte],
      contentType: MediaType = MediaType.OctetStream,
      filename: Option[String] = None,
  )

object Multipart:
  private val Crlf: Array[Byte]  = Array('\r', '\n').map(_.toByte)
  private val Close: Array[Byte] = Array('-', '-').map(_.toByte)

  /** A fresh boundary: 128 random bits, which no field's bytes will repeat by chance. */
  val boundary: UIO[String] =
    heddle.internal.Ids.token.map("----heddleFormBoundary" + _)

  def parse(bytes: Chunk[Byte], boundary: String): Either[MultipartError, Chunk[FormField]] =
    val (fields, _, err) = takeComplete(bytes, boundary, finish = true)
    err.toLeft(fields)

  def decode[E](stream: ZStream[Any, E, Byte], boundary: String): ZStream[Any, E | MultipartError, FormField] =
    ZStream.unwrap {
      Ref.make(Chunk.empty[Byte]).map { buf =>
        val live = stream.mapChunksZIO { chunk =>
          buf
            .modify { acc =>
              val (fields, rest, err) = takeComplete(acc ++ chunk, boundary, finish = false)
              err match
                case Some(e) => (Left(e), acc)
                case None    => (Right(fields), rest)
            }
            .flatMap(ZIO.fromEither(_))
        }
        val tail = ZStream.fromZIO(buf.get).flatMap { leftover =>
          val (fields, _, err) = takeComplete(leftover, boundary, finish = true)
          err match
            case Some(e) => ZStream.fail(e)
            case None    => ZStream.fromChunk(fields)
        }
        live ++ tail
      }
    }

  def encode(fields: Chunk[FormField], boundary: String): Chunk[Byte] =
    val dash                         = s"--$boundary"
    val b                            = Chunk.newBuilder[Byte]
    def utf8(s: String): Chunk[Byte] =
      Chunk.fromArray(s.getBytes(StandardCharsets.UTF_8))
    fields.foreach { field =>
      b ++= utf8(s"$dash\r\n")
      field match
        case FormField.Text(name, value, ct) =>
          b ++= utf8(s"Content-Disposition: form-data; name=${HeaderParams.quoted(name)}\r\n")
          ct.foreach(m => b ++= utf8(s"Content-Type: ${m.render}\r\n"))
          b ++= utf8("\r\n")
          b ++= Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
          b ++= utf8("\r\n")
        case FormField.Binary(name, data, ct, filename) =>
          val fn = filename.map(f => s"; filename=${HeaderParams.quoted(f)}").getOrElse("")
          b ++= utf8(s"Content-Disposition: form-data; name=${HeaderParams.quoted(name)}$fn\r\n")
          b ++= utf8(s"Content-Type: ${ct.render}\r\n\r\n")
          b ++= data
          b ++= utf8("\r\n")
      end match
    }
    b ++= utf8(s"$dash--\r\n")
    b.result()
  end encode

  private def takeComplete(
      bytes: Chunk[Byte],
      boundary: String,
      finish: Boolean,
  ): (Chunk[FormField], Chunk[Byte], Option[MultipartError]) =
    val raw   = bytes.toArray
    val dashB = ("--" + boundary).getBytes(StandardCharsets.US_ASCII)
    val sep   = ("\r\n--" + boundary).getBytes(StandardCharsets.US_ASCII)
    val first = indexOf(raw, dashB, 0)
    if first < 0 then
      if finish && bytes.nonEmpty then (Chunk.empty, Chunk.empty, Some(MultipartError.NoBoundary))
      else (Chunk.empty, bytes, None)
    else
      val out        = List.newBuilder[FormField]
      var from       = first + dashB.length
      var delimStart = first
      var keepFrom   = first
      var err        = Option.empty[MultipartError]
      var closed     = false
      while err.isEmpty && !closed && from <= raw.length do
        if startsWith(raw, from, Close) then
          closed = true
          keepFrom = raw.length
        else
          if startsWith(raw, from, Crlf) then from += 2
          val next = indexOf(raw, sep, from)
          if next < 0 then
            keepFrom = delimStart
            if finish then err = Some(MultipartError.Truncated)
            from = raw.length + 1
          else
            parsePart(raw, from, next).foreach(f => out += f)
            delimStart = next
            from = next + sep.length
            keepFrom = next
            if startsWith(raw, from, Close) then
              closed = true
              keepFrom = raw.length
          end if
      end while
      val rest =
        if keepFrom >= raw.length then Chunk.empty
        else Chunk.fromArray(Arrays.copyOfRange(raw, keepFrom, raw.length))
      (Chunk.fromIterable(out.result()), rest, err)
    end if
  end takeComplete

  /** A part without headers or a `Content-Disposition` name is skipped, as RFC 7578 §4.2 leaves it unnamed. */
  private def parsePart(raw: Array[Byte], from: Int, until: Int): Option[FormField] =
    val sep = indexOf(raw, "\r\n\r\n".getBytes(StandardCharsets.US_ASCII), from)
    if sep < 0 || sep >= until then None
    else
      val headers  = String(Arrays.copyOfRange(raw, from, sep), StandardCharsets.UTF_8)
      val bodyFrom = sep + 4
      val body     = Chunk.fromArray(Arrays.copyOfRange(raw, bodyFrom, math.max(bodyFrom, until)))
      disposition(headers).map { (name, fn) =>
        val ct = contentType(headers)
        fn match
          case Some(file) => FormField.Binary(name, body, ct.getOrElse(MediaType.OctetStream), Some(file))
          case None       => FormField.Text(name, String(body.toArray, StandardCharsets.UTF_8), ct)
      }
    end if
  end parsePart

  private def disposition(headers: String): Option[(String, Option[String])] =
    headers
      .split("\r\n")
      .find(_.toLowerCase.startsWith("content-disposition:"))
      .flatMap { line =>
        val params = line.indexOf(';') match
          case -1 => Map.empty[String, String]
          case at => HeaderParams.parse(line, at + 1).toMap
        params.get("name").map(_ -> params.get("filename"))
      }

  private def contentType(headers: String): Option[MediaType] =
    headers
      .split("\r\n")
      .find(_.toLowerCase.startsWith("content-type:"))
      .flatMap(line => MediaType.parse(line.substring(line.indexOf(':') + 1).trim))

  private def indexOf(hay: Array[Byte], needle: Array[Byte], from: Int): Int =
    val last = hay.length - needle.length
    var i    = from
    while i <= last && !startsWith(hay, i, needle) do i += 1
    if i <= last then i else -1

  private def startsWith(hay: Array[Byte], from: Int, needle: Array[Byte]): Boolean =
    if from < 0 || from + needle.length > hay.length then false
    else
      var j = 0
      while j < needle.length && hay(from + j) == needle(j) do j += 1
      j == needle.length
end Multipart
