package heddle.endpoint

import heddle.http.Method
import heddle.route.{Handler, Routes}
import heddle.route.PathDsl.*
object SwaggerUI:
  val defaultBundle: String = "https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js"
  val defaultCss: String    = "https://unpkg.com/swagger-ui-dist@5/swagger-ui.css"

  def routes(
      spec: OpenApi,
      prefix: String = "docs",
      bundleUrl: String = defaultBundle,
      cssUrl: String = defaultCss,
  ): Routes[Any, Nothing] =
    val base     = prefix.stripPrefix("/").stripSuffix("/")
    val specPath = s"/$base/openapi.json"
    val html     = page(specPath, spec.title, bundleUrl, cssUrl)
    val specJson = spec.toJson
    Routes(
      Method.GET / base / "openapi.json" -> Handler.json(specJson),
      Method.GET / base                  -> Handler.html(html),
      Method.GET / base / "index.html"   -> Handler.html(html),
    )
  end routes

  def page(
      specPath: String,
      title: String = "Heddle API",
      bundleUrl: String = defaultBundle,
      cssUrl: String = defaultCss,
  ): String =
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8"/>
       |  <title>$title</title>
       |  <link rel="stylesheet" href="$cssUrl">
       |</head>
       |<body>
       |  <div id="swagger-ui"></div>
       |  <script src="$bundleUrl"></script>
       |  <script>
       |    window.onload = () => {
       |      window.ui = SwaggerUIBundle({
       |        url: "$specPath",
       |        dom_id: "#swagger-ui",
       |        persistAuthorization: true
       |      });
       |    };
       |  </script>
       |</body>
       |</html>
       |""".stripMargin
end SwaggerUI
