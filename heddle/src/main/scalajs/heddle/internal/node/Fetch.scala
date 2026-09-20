package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array

class FetchInit(
    var method: js.UndefOr[String] = js.undefined,
    var headers: js.UndefOr[js.Dictionary[String]] = js.undefined,
    var body: js.UndefOr[Uint8Array] = js.undefined,
    var signal: js.UndefOr[AbortSignal] = js.undefined,
    var redirect: js.UndefOr[String] = js.undefined,
) extends js.Object

@js.native
trait AbortSignal extends js.Object

@js.native
@JSGlobal
class AbortController extends js.Object:
  def abort(): Unit       = js.native
  val signal: AbortSignal = js.native

@js.native
trait FetchHeaders extends js.Object:
  def forEach(cb: js.Function2[String, String, Unit]): Unit = js.native
  def get(name: String): String | Null                      = js.native

@js.native
trait ReadableStreamDefaultReader extends js.Object:
  def read(): js.Promise[StreamReadResult] = js.native

@js.native
trait StreamReadResult extends js.Object:
  val done: Boolean                 = js.native
  val value: js.UndefOr[Uint8Array] = js.native

@js.native
trait FetchBody extends js.Object:
  def arrayBuffer(): js.Promise[js.typedarray.ArrayBuffer] = js.native
  val body: ReadableStream | Null                          = js.native

@js.native
trait ReadableStream extends js.Object:
  def getReader(): ReadableStreamDefaultReader = js.native

@js.native
trait FetchResponse extends FetchBody:
  val status: Int           = js.native
  val statusText: String    = js.native
  val ok: Boolean           = js.native
  val headers: FetchHeaders = js.native
  val url: String           = js.native
end FetchResponse

@js.native
@JSGlobal("fetch")
private[heddle] def fetch(url: String, init: FetchInit): js.Promise[FetchResponse] = js.native
