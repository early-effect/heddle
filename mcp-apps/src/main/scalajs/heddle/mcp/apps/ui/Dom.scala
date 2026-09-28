package heddle.mcp.apps.ui

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSName}

/** The few DOM facades a relay and a host frame use. Heddle's own, so it does not depend on a DOM library. A field the
  * browser may leave `null` is read through an `Option` beside it, and nothing nullable leaves this file.
  */
@js.native
private[heddle] trait Element extends js.Object:
  var textContent: String                                                           = js.native
  def setAttribute(name: String, value: String): Unit                               = js.native
  def appendChild(child: Element): Element                                          = js.native
  def remove(): Unit                                                                = js.native
  def addEventListener(kind: String, listener: js.Function1[js.Any, Unit]): Unit    = js.native
  def removeEventListener(kind: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

/** An `<iframe>`. */
@js.native
private[heddle] trait IFrame extends Element:
  var srcdoc: String = js.native
  var src: String    = js.native
  @JSName("contentWindow")
  val contentWindowOrNull: Window = js.native

private[heddle] object IFrame:
  extension (f: IFrame)
    /** `None` until the frame is in a document. */
    def contentWindow: Option[Window] = Option(f.contentWindowOrNull)

@js.native
private[heddle] trait Document extends js.Object:
  val body: Element                       = js.native
  def createElement(tag: String): Element = js.native
  @JSName("createElement")
  def createIFrame(tag: "iframe"): IFrame = js.native
  @JSName("getElementById")
  def elementByIdOrNull(id: String): Element = js.native

private[heddle] object Document:
  extension (d: Document)
    def elementById(id: String): Option[Element] = Option(d.elementByIdOrNull(id))
    def iframe: IFrame                           = d.createIFrame("iframe")

@js.native
private[heddle] trait Location extends js.Object:
  val href: String   = js.native
  val origin: String = js.native

/** The window a script runs in. Reading `top.location.href` throws `SecurityError` when `top` is on another origin. */
@js.native
private[heddle] trait SelfWindow extends Window:
  val top: SelfWindow                                           = js.native
  val self: SelfWindow                                          = js.native
  val location: Location                                        = js.native
  def open(url: String, target: String, features: String): Unit = js.native

@js.native
@JSGlobal("document")
private[heddle] object BrowserDocument extends Document

@js.native
@JSGlobal("window")
private[heddle] object BrowserSelf extends SelfWindow
