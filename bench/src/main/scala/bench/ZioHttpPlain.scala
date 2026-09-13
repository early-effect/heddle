package bench

import zio.*
import zio.http.*

object ZioHttpPlain extends ZIOAppDefault:
  private val hello = "Hello, World!"

  private val routes =
    Routes(
      Method.GET / "plaintext" -> handler(Response.text(hello)),
      Method.GET / "json"      -> handler(Response.json("""{"message":"Hello, World!"}""")),
      Method.POST / "echo"     -> handler { (req: Request) =>
        req.body.asChunk.map(ch => Response(body = Body.fromChunk(ch))).orDie
      },
    )

  def run =
    for
      args <- getArgs
      port = args.headOption.flatMap(_.toIntOption).getOrElse(8089)
      _ <- ZIO.logInfo(s"zio-http plaintext on :$port")
      _ <- Server.serve(routes).provide(Server.defaultWithPort(port))
    yield ()
end ZioHttpPlain
