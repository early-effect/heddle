package heddle.mcp.events

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

import java.io.{InputStream, OutputStream}
import java.net.{
  ConnectException,
  InetAddress,
  InetSocketAddress,
  Socket,
  SocketTimeoutException,
  URI,
  UnknownHostException,
}
import java.nio.charset.StandardCharsets
import javax.net.ssl.{SNIHostName, SSLException, SSLSocket, SSLSocketFactory}
import scala.util.Try

/** JVM HTTPS sender. `attemptBlocking` is the impure edge: the socket timeout bounds the block, so a peer that never
  * answers cannot hold the fiber. The TLS handshake itself is not covered by a test.
  */
private[events] object WebhookTransportPlatform:
  private val TimeoutMs = 5000
  private val BodyCap   = 64 * 1024

  val live: ULayer[WebhookTransport] =
    ZLayer.succeed(new WebhookTransport:
      def post(call: WebhookCall): IO[DeliveryFault, DeliveryReply] =
        ZIO.attemptBlocking(exchange(call)).mapError(classify).flatMap(ZIO.fromEither))

  private def classify(thrown: Throwable): DeliveryFault =
    thrown match
      case _: SocketTimeoutException                     => DeliveryFault.TimedOut
      case _: ConnectException | _: UnknownHostException => DeliveryFault.ConnectionRefused
      case _: SSLException                               => DeliveryFault.Tls
      case _: java.security.cert.CertificateException    => DeliveryFault.Tls
      case _                                             => DeliveryFault.Unavailable

  private def exchange(call: WebhookCall): Either[DeliveryFault, DeliveryReply] =
    Try(new URI(call.url)).toOption match
      case None      => Left(DeliveryFault.Rejected)
      case Some(uri) =>
        if uri.getScheme != "https" || Option(uri.getUserInfo).exists(_.nonEmpty) then Left(DeliveryFault.Rejected)
        else
          Option(uri.getHost).filter(_.nonEmpty) match
            case None       => Left(DeliveryFault.Rejected)
            case Some(host) =>
              val port = if uri.getPort == -1 then 443 else uri.getPort
              resolve(host).flatMap { addresses =>
                addresses.headOption match
                  case None          => Left(DeliveryFault.Rejected)
                  case Some(address) =>
                    SSLSocketFactory.getDefault match
                      case factory: SSLSocketFactory =>
                        factory.createSocket() match
                          case socket: SSLSocket =>
                            try deliver(socket, uri, host, port, address, call)
                            finally close(socket)
                          case other =>
                            try Left(DeliveryFault.Unavailable)
                            finally close(other)
                      case _ => Left(DeliveryFault.Unavailable)
              }

  private def resolve(host: String): Either[DeliveryFault, List[InetAddress]] =
    val found = InetAddress.getAllByName(host).toList
    val bytes = found.map(address => Chunk.fromArray(address.getAddress))
    if bytes.exists(raw => !AddressPolicy.globallyRoutable(raw)) then Left(DeliveryFault.Rejected)
    else Right(found)

  private def deliver(
      socket: SSLSocket,
      uri: URI,
      host: String,
      port: Int,
      address: InetAddress,
      call: WebhookCall,
  ): Either[DeliveryFault, DeliveryReply] =
    socket.connect(new InetSocketAddress(address, port), TimeoutMs)
    socket.setSoTimeout(TimeoutMs)
    if AddressPolicy.parseLiteral(host).isEmpty then
      val params = socket.getSSLParameters
      params.setServerNames(java.util.List.of(new SNIHostName(host)))
      params.setEndpointIdentificationAlgorithm("HTTPS")
      socket.setSSLParameters(params)
    socket.startHandshake()
    write(socket.getOutputStream, requestBytes(uri, host, port, call))
    readReply(socket.getInputStream)

  private def requestBytes(uri: URI, host: String, port: Int, call: WebhookCall): Array[Byte] =
    val path       = Option(uri.getRawPath).filter(_.nonEmpty).getOrElse("/")
    val query      = Option(uri.getRawQuery).fold("")(q => s"?$q")
    val hostHeader = if port == 443 then host else if host.contains(':') then s"[$host]:$port" else s"$host:$port"
    val body       = call.body.getBytes(StandardCharsets.UTF_8)
    val head       = new StringBuilder
    head.append(s"POST $path$query HTTP/1.1\r\n")
    head.append(s"Host: $hostHeader\r\n")
    head.append("User-Agent: heddle-mcp-events\r\n")
    head.append("Accept: application/json\r\n")
    head.append(s"Content-Length: ${body.length}\r\n")
    head.append("Connection: close\r\n")
    call.headers.foreach { (name, value) =>
      head.append(name)
      head.append(": ")
      head.append(value)
      head.append("\r\n")
    }
    head.append("\r\n")
    head.toString.getBytes(StandardCharsets.UTF_8) ++ body

  private def write(out: OutputStream, bytes: Array[Byte]): Unit =
    out.write(bytes)
    out.flush()

  private def readReply(in: InputStream): Either[DeliveryFault, DeliveryReply] =
    readLine(in).flatMap { status =>
      val code = status.split(' ').drop(1).headOption.flatMap(token => Try(token.toInt).toOption)
      code match
        case None             => Left(DeliveryFault.Unavailable)
        case Some(statusCode) =>
          readHeaders(in, Map.empty).flatMap { headers =>
            val length  = headers.get("content-length").flatMap(raw => Try(raw.toInt).toOption)
            val chunked = headers.get("transfer-encoding").exists(_.toLowerCase.contains("chunked"))
            val body    =
              if chunked then readChunked(in)
              else
                length match
                  case Some(n) => readCounted(in, math.min(n, BodyCap))
                  case None    => readUntilClose(in)
            body.map(text => DeliveryReply(statusCode, text))
          }
    }

  private def readHeaders(in: InputStream, acc: Map[String, String]): Either[DeliveryFault, Map[String, String]] =
    readLine(in).flatMap { line =>
      if line.isEmpty then Right(acc)
      else
        val cut = line.indexOf(':')
        if cut <= 0 then readHeaders(in, acc)
        else
          val name  = line.substring(0, cut).trim.toLowerCase
          val value = line.substring(cut + 1).trim
          readHeaders(in, acc.updated(name, value))
    }

  private def readLine(in: InputStream): Either[DeliveryFault, String] =
    val out                                         = new Array[Byte](8 * 1024)
    def loop(n: Int): Either[DeliveryFault, String] =
      if n >= out.length then Left(DeliveryFault.Unavailable)
      else
        in.read() match
          case -1 =>
            if n == 0 then Left(DeliveryFault.Unavailable)
            else Right(new String(out, 0, n, StandardCharsets.UTF_8))
          case 10 =>
            val end = if n > 0 && out(n - 1) == '\r'.toByte then n - 1 else n
            Right(new String(out, 0, end, StandardCharsets.UTF_8))
          case other =>
            out(n) = other.toByte
            loop(n + 1)
    loop(0)

  private def readCounted(in: InputStream, count: Int): Either[DeliveryFault, String] =
    val out                                           = new Array[Byte](count)
    def loop(got: Int): Either[DeliveryFault, String] =
      if got >= count then Right(new String(out, StandardCharsets.UTF_8))
      else
        val n = in.read(out, got, count - got)
        if n < 0 then Right(new String(out, 0, got, StandardCharsets.UTF_8))
        else loop(got + n)
    loop(0)

  private def readUntilClose(in: InputStream): Either[DeliveryFault, String] =
    readCounted(in, BodyCap)

  private def readChunked(in: InputStream): Either[DeliveryFault, String] =
    val sink                                = new java.io.ByteArrayOutputStream
    def one: Either[DeliveryFault, Boolean] =
      readLine(in).flatMap { line =>
        val sizeHex = line.takeWhile(_ != ';').trim
        Try(Integer.parseInt(sizeHex, 16)).toOption match
          case None                    => Left(DeliveryFault.Unavailable)
          case Some(size) if size <= 0 => Right(false)
          case Some(size)              =>
            val room = math.min(size, BodyCap - sink.size)
            readCounted(in, room).flatMap { text =>
              sink.write(text.getBytes(StandardCharsets.UTF_8))
              val skip = size - room
              if skip > 0 then discard(in, skip)
              readLine(in).map(_ => sink.size < BodyCap)
            }
      }
    def pump: Either[DeliveryFault, String] =
      one.flatMap { more =>
        if more then pump else Right(sink.toString(StandardCharsets.UTF_8))
      }
    pump

  private def discard(in: InputStream, count: Int): Either[DeliveryFault, Unit] =
    val buf                                          = new Array[Byte](1024)
    def loop(left: Int): Either[DeliveryFault, Unit] =
      if left <= 0 then Right(())
      else
        val n = in.read(buf, 0, math.min(buf.length, left))
        if n < 0 then Right(()) else loop(left - n)
    loop(count)

  private def close(socket: Socket): Unit =
    try socket.close()
    catch case _: java.io.IOException => ()
end WebhookTransportPlatform
