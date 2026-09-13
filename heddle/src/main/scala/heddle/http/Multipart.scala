package heddle.http

import java.nio.charset.StandardCharsets
import java.util.Arrays
import zio.{Chunk, Ref}
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

  def boundary(): String =
    "----heddleFormBoundary" + java.util.UUID.randomUUID().toString.replace("-", "")

  def parse(bytes: Chunk[Byte], boundary: String): Either[String, Chunk[FormField]] =
    val (fields, _, err) = takeComplete(bytes, boundary, finish = true)
    err.toLeft(fields)

  def decode(stream: ZStream[Any, Throwable, Byte], boundary: String): ZStream[Any, Throwable, FormField] =
    ZStream.unwrap {
      Ref.make(Chunk.empty[Byte]).map { buf =>
        val live = stream.mapChunksZIO { chunk =>
          buf.modify { acc =>
            val (fields, rest, err) = takeComplete(acc ++ chunk, boundary, finish = false)
            err match
              case Some(msg) => throw IllegalArgumentException(msg)
              case None      => (fields, rest)
          }
        }
        val tail = ZStream.fromZIO(buf.get).flatMap { leftover =>
          val (fields, _, err) = takeComplete(leftover, boundary, finish = true)
          err match
            case Some(msg) => ZStream.fail(IllegalArgumentException(msg))
            case None      => ZStream.fromChunk(fields)
        }
        live ++ tail
      }
    }

  def encode(fields: Chunk[FormField], boundary: String): Chunk[Byte] =
    val dash                          = s"--$boundary"
    val b                             = Chunk.newBuilder[Byte]
    def ascii(s: String): Chunk[Byte] =
      Chunk.fromArray(s.getBytes(StandardCharsets.US_ASCII))
    fields.foreach { field =>
      b ++= ascii(s"$dash\r\n")
      field match
        case FormField.Text(name, value, ct) =>
          b ++= ascii(s"""Content-Disposition: form-data; name="$name"\r\n""")
          ct.foreach(m => b ++= ascii(s"Content-Type: ${m.render}\r\n"))
          b ++= ascii("\r\n")
          b ++= Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
          b ++= ascii("\r\n")
        case FormField.Binary(name, data, ct, filename) =>
          val fn = filename.map(f => s"""; filename="$f"""").getOrElse("")
          b ++= ascii(s"""Content-Disposition: form-data; name="$name"$fn\r\n""")
          b ++= ascii(s"Content-Type: ${ct.render}\r\n\r\n")
          b ++= data
          b ++= ascii("\r\n")
      end match
    }
    b ++= ascii(s"$dash--\r\n")
    b.result()
  end encode

  private def takeComplete(
      bytes: Chunk[Byte],
      boundary: String,
      finish: Boolean,
  ): (Chunk[FormField], Chunk[Byte], Option[String]) =
    val raw   = bytes.toArray
    val dashB = ("--" + boundary).getBytes(StandardCharsets.US_ASCII)
    val sep   = ("\r\n--" + boundary).getBytes(StandardCharsets.US_ASCII)
    val first = indexOf(raw, dashB, 0)
    if first < 0 then
      if finish && bytes.nonEmpty then (Chunk.empty, Chunk.empty, Some("missing multipart boundary"))
      else (Chunk.empty, bytes, None)
    else
      val out        = List.newBuilder[FormField]
      var from       = first + dashB.length
      var delimStart = first
      var keepFrom   = first
      var err        = Option.empty[String]
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
            if finish then err = Some("truncated multipart body")
            from = raw.length + 1
          else
            parsePart(raw, from, next) match
              case Left(msg)      => err = Some(msg)
              case Right(None)    => ()
              case Right(Some(f)) => out += f
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

  private def parsePart(raw: Array[Byte], from: Int, until: Int): Either[String, Option[FormField]] =
    val sep = indexOf(raw, "\r\n\r\n".getBytes(StandardCharsets.US_ASCII), from)
    if sep < 0 || sep >= until then Right(None)
    else
      val headers   = String(Arrays.copyOfRange(raw, from, sep), StandardCharsets.US_ASCII)
      val bodyFrom  = sep + 4
      val bodyUntil =
        if until >= 2 && raw(until - 2) == '\r' && raw(until - 1) == '\n' then until - 2 else until
      val body = Chunk.fromArray(Arrays.copyOfRange(raw, bodyFrom, math.max(bodyFrom, bodyUntil)))
      disposition(headers) match
        case None             => Right(None)
        case Some((name, fn)) =>
          val ct = contentType(headers)
          Right(
            Some(
              fn match
                case Some(file) =>
                  FormField.Binary(name, body, ct.getOrElse(MediaType.OctetStream), Some(file))
                case None =>
                  FormField.Text(name, String(body.toArray, StandardCharsets.UTF_8), ct)
            )
          )
      end match
    end if
  end parsePart

  private def disposition(headers: String): Option[(String, Option[String])] =
    headers
      .split("\r\n")
      .find(_.toLowerCase.startsWith("content-disposition:"))
      .flatMap { line =>
        val rest = line.substring(line.indexOf(':') + 1)
        param(rest, "name").map(_ -> param(rest, "filename"))
      }

  private def contentType(headers: String): Option[MediaType] =
    headers
      .split("\r\n")
      .find(_.toLowerCase.startsWith("content-type:"))
      .flatMap(line => MediaType.parse(line.substring(line.indexOf(':') + 1).trim))

  private def param(header: String, key: String): Option[String] =
    val needle = key + "="
    val idx    = header.toLowerCase.indexOf(needle)
    if idx < 0 then None
    else
      var i = idx + needle.length
      while i < header.length && header.charAt(i) == ' ' do i += 1
      if i < header.length && header.charAt(i) == '"' then
        val close = header.indexOf('"', i + 1)
        if close < 0 then Some(header.substring(i + 1)) else Some(header.substring(i + 1, close))
      else
        val end = header.indexOf(';', i)
        Some((if end < 0 then header.substring(i) else header.substring(i, end)).trim)
    end if
  end param

  private def indexOf(hay: Array[Byte], needle: Array[Byte], from: Int): Int =
    var i = from
    while i <= hay.length - needle.length do
      var j    = 0
      var same = true
      while same && j < needle.length do
        if hay(i + j) != needle(j) then same = false
        else j += 1
      if same then return i
      i += 1
    -1
  end indexOf

  private def startsWith(hay: Array[Byte], from: Int, needle: Array[Byte]): Boolean =
    from >= 0 && from + needle.length <= hay.length && needle.indices.forall(j => hay(from + j) == needle(j))
end Multipart
