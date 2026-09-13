package heddle.json

import heddle.endpoint.JsonCodec
import zio.json.{JsonDecoder, JsonEncoder}

given zioJsonCodec[A](using enc: JsonEncoder[A], dec: JsonDecoder[A]): JsonCodec[A] =
  JsonCodec.from(
    a => enc.encodeJson(a).toString,
    s => dec.decodeJson(s),
  )
