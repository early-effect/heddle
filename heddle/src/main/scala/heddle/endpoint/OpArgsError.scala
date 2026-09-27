package heddle.endpoint

import heddle.error.HeddleError

/** Why an endpoint's input and a flat JSON arguments object (an MCP tool call) do not convert. */
enum OpArgsError(val message: String) extends HeddleError:
  case NonJsonBody  extends OpArgsError("a body that is not JSON has no argument form")
  case StreamedBody extends OpArgsError("a streamed body has no argument form")
  case NameCollision(names: List[String])
      extends OpArgsError(s"path, query, and body argument names collide: ${names.mkString(", ")}")
  case NotAnObject extends OpArgsError("arguments must be a JSON object")
  case Missing(location: ParamLocation, name: String)
      extends OpArgsError(s"missing ${location.render} argument '$name'")
  case NotScalar(location: ParamLocation, name: String)
      extends OpArgsError(s"${location.render} argument '$name' must be a string, number, or boolean")
  case MissingBody                                  extends OpArgsError("missing body arguments")
  case BodyNotJson(detail: String)                  extends OpArgsError(s"the body is not JSON: $detail")
  case PathMismatch(path: String, template: String) extends OpArgsError(s"$path does not match $template")
  case RepeatedQuery(name: String) extends OpArgsError(s"query parameter '$name' repeats; an argument holds one value")
end OpArgsError
