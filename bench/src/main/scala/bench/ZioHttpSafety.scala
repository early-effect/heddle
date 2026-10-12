package bench

import zio.*
import zio.http.*

/** Safety probe target on the same [[SafetyProfile]] as heddle. zio-http has no connection cap. */
object ZioHttpSafety extends ZIOAppDefault:
  private val hello = "Hello, World!"

  private val routes =
    Routes(
      Method.GET / "plaintext" -> handler(Response.text(hello)),
      Method.POST / "echo"     -> handler { (req: Request) =>
        req.body.asChunk.map(ch => Response(body = Body.fromChunk(ch))).orDie
      },
    )

  def run =
    for
      args <- getArgs
      port = args.headOption.flatMap(_.toIntOption).getOrElse(8089)
      _ <- ZIO.logInfo(SafetyProfile.describe)
      _ <- ZIO.logInfo("zio-http safety maxConnections=unset")
      _ <- Server
        .serve(routes)
        .provide(
          Server.defaultWith(cfg =>
            cfg
              .binding("127.0.0.1", port)
              .maxHeaderSize(SafetyProfile.headerBytes)
              .maxInitialLineLength(SafetyProfile.headerBytes)
              .disableRequestStreaming(SafetyProfile.bodyBytes.toInt)
              .idleTimeout(SafetyProfile.readTimeout)
              .soBacklog(SafetyProfile.backlog)
              .tcpNoDelay(true)
          )
        )
    yield ()
end ZioHttpSafety
