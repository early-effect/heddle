package bench

import heddle.*
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
      port = args.headOption.flatMap(_.toIntOption).getOrElse(8088)
      _ <- ZIO.logInfo(s"heddle plaintext on :$port")
      _ <- Server.serve(routes, Server.Config.default.copy(host = "127.0.0.1", port = port))
    yield ()
end HeddlePlain
