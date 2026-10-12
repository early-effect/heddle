package bench

import heddle.*
import heddle.server.{Backlog, BodyBytes, HeaderBytes, MaxConnections, Port}
import zio.*

/** Safety probe target. Not the plaintext bench: caps and deadlines are [[SafetyProfile]], not production defaults. */
object HeddleSafety extends ZIOAppDefault:
  private val hello = "Hello, World!"

  private val routes =
    Routes(
      Method.GET / "plaintext" -> Handler.text(hello),
      Method.POST / "echo"     -> handler { (req: Request) => ZIO.succeed(Response(Status.Ok).withBody(req.body)) },
    )

  def run =
    for
      args <- getArgs
      raw = args.headOption.flatMap(_.toIntOption).getOrElse(8088)
      port           <- ZIO.fromEither(Port.make(raw))
      headerBytes    <- ZIO.fromEither(HeaderBytes.make(SafetyProfile.headerBytes))
      bodyBytes      <- ZIO.fromEither(BodyBytes.make(SafetyProfile.bodyBytes))
      maxConnections <- ZIO.fromEither(MaxConnections.make(SafetyProfile.maxConnections))
      backlog        <- ZIO.fromEither(Backlog.make(SafetyProfile.backlog))
      _              <- ZIO.logInfo(SafetyProfile.describe)
      _              <- ZIO.logInfo(s"heddle safety maxConnections=${SafetyProfile.maxConnections}")
      _              <- Server.serve(
        routes,
        Server.Config.default.copy(
          host = "127.0.0.1",
          port = port,
          maxHeaderBytes = headerBytes,
          maxBodyBytes = bodyBytes,
          headerTimeout = SafetyProfile.readTimeout,
          idleTimeout = SafetyProfile.readTimeout,
          maxConnections = maxConnections,
          soBacklog = backlog,
          tcpNoDelay = true,
        ),
      )
    yield ()
end HeddleSafety
