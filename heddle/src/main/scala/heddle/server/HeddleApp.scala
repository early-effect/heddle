package heddle.server

import heddle.Server
import heddle.http.Response
import heddle.route.Routes
import zio.*

/** ZIOApp that serves [[routes]] and exits 0 on Ctrl-C under sbt 2. */
trait HeddleApp extends ZIOAppDefault:
  def routes: Routes[Any, Response]

  def config: Server.Config = Server.Config.default

  def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    Server.sbtInterruptExit *>
      Server
        .serve(routes, config)
        .catchAllCause(c => if c.isInterruptedOnly then ZIO.unit else ZIO.refailCause(c))
