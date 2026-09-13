package heddle.sse

import java.nio.charset.StandardCharsets
import zio.{Chunk, Duration}

object SseCodec:
  val heartbeat: Chunk[Byte] = Chunk.fromArray(": ping\n\n".getBytes(StandardCharsets.US_ASCII))

  def encode(event: ServerSentEvent): Chunk[Byte] =
    if event eq ServerSentEvent.Heartbeat then heartbeat
    else
      val b = Array.newBuilder[Byte]
      event.event.foreach { name =>
        putAscii(b, "event: ")
        putUtf8(b, name)
        b += '\n'
      }
      splitData(event.data).foreach { line =>
        putAscii(b, "data: ")
        putUtf8(b, line)
        b += '\n'
      }
      event.id.foreach { id =>
        putAscii(b, "id: ")
        putUtf8(b, id)
        b += '\n'
      }
      event.retry.foreach { d =>
        putAscii(b, "retry: ")
        putAscii(b, d.toMillis.toString)
        b += '\n'
      }
      b += '\n'
      Chunk.fromArray(b.result())

  /** Complete events in `bytes`, plus leftover that is not yet a blank-line-terminated event. */
  def decode(bytes: Chunk[Byte]): (Chunk[ServerSentEvent], Chunk[Byte]) =
    val raw          = bytes.toArray
    val events       = Chunk.newBuilder[ServerSentEvent]
    var start        = 0
    var i            = 0
    var lastComplete = 0
    while i < raw.length do
      val blank = blankLineAt(raw, i)
      if blank < 0 then i += 1
      else
        parseEvent(raw, start, i).foreach(events += _)
        i += blank
        start = i
        lastComplete = i
    (events.result(), bytes.drop(lastComplete))
  end decode

  private def splitData(data: String): List[String] =
    if data.isEmpty then List("")
    else
      val buf   = List.newBuilder[String]
      var start = 0
      var i     = 0
      while i < data.length do
        val c = data.charAt(i)
        if c == '\r' then
          buf += data.substring(start, i)
          if i + 1 < data.length && data.charAt(i + 1) == '\n' then i += 2
          else i += 1
          start = i
        else if c == '\n' then
          buf += data.substring(start, i)
          i += 1
          start = i
        else i += 1
      buf += data.substring(start)
      buf.result()

  /** Length of a blank-line terminator starting at `i`, or -1. */
  private def blankLineAt(raw: Array[Byte], i: Int): Int =
    def isLf(j: Int)   = j < raw.length && raw(j) == '\n'
    def isCrLf(j: Int) = j + 1 < raw.length && raw(j) == '\r' && raw(j + 1) == '\n'
    if isCrLf(i) && isCrLf(i + 2) then 4
    else if isCrLf(i) && isLf(i + 2) then 3
    else if isLf(i) && isLf(i + 1) then 2
    else if isLf(i) && isCrLf(i + 1) then 3
    else -1

  private def parseEvent(raw: Array[Byte], from: Int, until: Int): Option[ServerSentEvent] =
    var data: Option[String]    = None
    var event: Option[String]   = None
    var id: Option[String]      = None
    var retry: Option[Duration] = None
    var i                       = from
    while i < until do
      var eol = i
      while eol < until && raw(eol) != '\n' && raw(eol) != '\r' do eol += 1
      val lineEnd = eol
      if eol < until && raw(eol) == '\r' then eol += 1
      if eol < until && raw(eol) == '\n' then eol += 1
      if lineEnd > i && raw(i) != ':' then
        var colon = i
        while colon < lineEnd && raw(colon) != ':' do colon += 1
        val name  = String(raw, i, colon - i, StandardCharsets.US_ASCII)
        val vFrom =
          if colon < lineEnd then
            val after = colon + 1
            if after < lineEnd && raw(after) == ' ' then after + 1 else after
          else lineEnd
        val value = String(raw, vFrom, lineEnd - vFrom, StandardCharsets.UTF_8)
        name match
          case "data" =>
            data = data match
              case None    => Some(value)
              case Some(d) => Some(d + "\n" + value)
          case "event" => event = Some(value)
          case "id"    => id = Some(value)
          case "retry" =>
            value.toLongOption.filter(_ >= 0).foreach(ms => retry = Some(Duration.fromMillis(ms)))
          case _ => ()
      end if
      i = eol
    end while
    if data.isEmpty && event.isEmpty && id.isEmpty && retry.isEmpty then None
    else Some(ServerSentEvent(data.getOrElse(""), event, id, retry))
  end parseEvent

  private def putAscii(b: scala.collection.mutable.ArrayBuilder[Byte], s: String): Unit =
    var i = 0
    while i < s.length do
      b += s.charAt(i).toByte
      i += 1

  private def putUtf8(b: scala.collection.mutable.ArrayBuilder[Byte], s: String): Unit =
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    var i     = 0
    while i < bytes.length do
      b += bytes(i)
      i += 1
end SseCodec
