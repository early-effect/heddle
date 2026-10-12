package bench

import heddle.*
import heddle.server.Port
import zio.*

object HeddlePlain extends ZIOAppDefault:
  private val hello = "Hello, World!"

  private val routes =
    Routes(
      Method.GET / "plaintext" -> Handler.text(hello),
      Method.GET / "json"      -> Handler.json("""{"message":"Hello, World!"}"""),
      Method.POST / "echo"     -> handler { (req: Request) => ZIO.succeed(Response(Status.Ok).withBody(req.body)) },
    )

  def run =
    for
      args <- getArgs
      raw = args.headOption.flatMap(_.toIntOption).getOrElse(8088)
      port <- ZIO.fromEither(Port.make(raw))
      _    <- ZIO.logInfo(s"heddle plaintext on :${port.toInt}")
      _    <- Server.serve(routes, Server.Config.default.copy(host = "127.0.0.1", port = port))
    yield ()
end HeddlePlain
