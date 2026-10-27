package heddle

import zio.*
import zio.test.*

object AroundSpec extends ZIOSpecDefault:
  private def recording(seen: Ref[Chunk[Exchange]]): Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        seen.update(_ :+ exchange) *> next(exchange.request)

  private def tagging(value: String): Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        next(exchange.request.withHeader("X-Tag", value))

  private def logging(log: Ref[Chunk[String]], name: String): Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        log.update(_ :+ s"$name in") *> next(exchange.request) <* log.update(_ :+ s"$name out")

  private val users =
    Routes(Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) })

  private val echoTag =
    Routes(
      Method.GET / "tag" -> handler((req: Request) => ZIO.succeed(Response.text(req.header("X-Tag").getOrElse(""))))
    )

  private val server =
    suite("on a server")(
      test("sees the matched route's template, not the raw path"):
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          res  <- (users @@ Middleware.around(recording(seen)))(Request.get("/users/7"))
          all  <- seen.get
        yield assertTrue(
          res.body.text.contains("7"),
          all.map(_.side) == Chunk(Side.Server),
          all.flatMap(_.route) == Chunk(RouteName(Method.GET, "/users/{id}")),
          all.map(_.request.path.render) == Chunk("/users/7"),
        )
      ,
      test("a miss is not intercepted, and a later route still answers it"):
        val other = Routes(Method.GET / "other" -> Handler.text("other"))
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          res  <- ((users @@ Middleware.around(recording(seen))) ++ other)(Request.get("/other"))
          all  <- seen.get
        yield assertTrue(res.body.text.contains("other"), all.isEmpty)
      ,
      test("a wrong method is a 405 the middleware never sees"):
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          res  <- (users @@ Middleware.around(recording(seen)))(Request(Method.POST, Url.parse("/users/7")))
          all  <- seen.get
        yield assertTrue(res.status == Status.MethodNotAllowed, all.isEmpty)
      ,
      test("hands the route a changed request"):
        (echoTag @@ Middleware.around(tagging("server")))(Request.get("/tag")).map { res =>
          assertTrue(res.body.text.contains("server"))
        }
      ,
      test("a ++ b runs a outside b"):
        for
          log <- Ref.make(Chunk.empty[String])
          _   <- (users @@ Middleware.around(logging(log, "a") ++ logging(log, "b")))(Request.get("/users/1"))
          all <- log.get
        yield assertTrue(all == Chunk("a in", "b in", "b out", "a out")),
    )

  private val client =
    suite("on a client")(
      test("sees the client side and no route"):
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          res  <- ZIO
            .serviceWithZIO[Client](_ @@ recording(seen))
            .flatMap(_.batched(Request.get("http://api.example/users/7")))
          all <- seen.get
        yield assertTrue(
          res.body.text.contains("7"),
          all.map(_.side) == Chunk(Side.Client),
          all.flatMap(_.route).isEmpty,
          all.flatMap(_.request.url.host) == Chunk("api.example"),
        )
      ,
      test("the changed request is what goes out"):
        ZIO
          .serviceWithZIO[Client](_.batched(Request.get("/tag")))
          .map(res => assertTrue(res.body.text.contains("client")))
          .provideLayer(Client.inMemory(echoTag) >>> Client.around(tagging("client")))
      ,
      test("a streaming call goes through it too"):
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          _    <- ZIO.scoped(
            ZIO.serviceWithZIO[Client](_ @@ recording(seen)).flatMap(_.streaming(Request.get("/users/7")))
          )
          all <- seen.get
        yield assertTrue(all.map(_.side) == Chunk(Side.Client)),
    ).provideLayer(Client.inMemory(users))

  def spec =
    suite("Around")(
      server,
      client,
      test("one value runs on both sides"):
        for
          seen <- Ref.make(Chunk.empty[Exchange])
          around = recording(seen)
          routes = users @@ Middleware.around(around)
          _ <- ZIO
            .serviceWithZIO[Client](_ @@ around)
            .flatMap(_.batched(Request.get("/users/7")))
            .provideLayer(Client.inMemory(routes))
          all <- seen.get
        yield assertTrue(all.map(_.side) == Chunk(Side.Client, Side.Server)),
    )
end AroundSpec
