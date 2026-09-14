package heddle.docs.widget

import ascent.*
import ascent.dsl.*
import specular.client.Mounter
import zio.*

object OpenApiPreview:
  val mounter: Mounter = Mounter.fromAscent(ui)

  // specular:begin demo
  private def ui: URIO[Scope, UI[Any]] =
    for op <- sq("get")
    yield
      val yaml = op.map {
        case "post" =>
          """/users:
  post:
    summary: Create a user
    requestBody:
      content:
        application/json:
          schema: NewUser
    responses:
      "201": { schema: User }"""
        case "list" =>
          """/users:
  get:
    summary: List users
    responses:
      "200": { schema: User[] }"""
        case _ =>
          """/users/{id}:
  get:
    summary: Get a user
    parameters:
      - name: id
        in: path
        required: true
        schema: integer
    responses:
      "200": { schema: User }
      "404": { schema: NotFound }"""
      }
      E.div(
        DocsUi.Lab,
        E.div(
          DocsUi.Row,
          DocsUi.modeButton(op, "get"),
          DocsUi.modeButton(op, "list"),
          DocsUi.modeButton(op, "post"),
        ),
        E.pre(DocsUi.Mono, yaml),
        E.div(DocsUi.Hint, "OpenAPI is a projection of EndpointDoc. spec.routes(\"docs\") serves Swagger."),
      )
  // specular:end
end OpenApiPreview
