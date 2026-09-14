package heddle.docs.widget

import ascent.*
import ascent.dsl.*

object DocsUi:
  val ink     = Color.hex("#1f2328")
  val muted   = Color.hex("#656d76")
  val line    = Color.hex("#d0d7de")
  val panel   = Color.hex("#f6f8fa")
  val surface = Color.hex("#ffffff")
  val accent  = Color.hex("#0969da")
  val ok      = Color.hex("#1a7f37")

  object Lab
      extends CssClass(
        S.display.grid,
        S.gap(0.75.rem),
        S.padding(1.rem),
        S.background(panel),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(8),
        S.color(ink),
        Declaration("font-family", "'IBM Plex Sans', 'Segoe UI', sans-serif"),
        S.fontSize.px(14),
      )

  object Row
      extends CssClass(
        S.display.flex,
        S.flexWrap.wrap,
        S.alignItems.center,
        S.gap(0.5.rem),
      )

  object Btn
      extends CssClass(
        S.padding(0.35.rem, 0.75.rem),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(6),
        S.background(surface),
        S.color(ink),
        S.cursor.pointer,
        S.fontWeight(500),
      )

  object BtnOn
      extends CssClass(
        S.padding(0.35.rem, 0.75.rem),
        S.border(Border.solid(1.px, accent)),
        S.borderRadius.px(6),
        S.background(accent),
        S.color(Color.hex("#ffffff")),
        S.cursor.pointer,
        S.fontWeight(600),
      )

  object Mono
      extends CssClass(
        Declaration("font-family", "'IBM Plex Mono', ui-monospace, monospace"),
        S.fontSize.px(12),
        S.whiteSpace.preWrap,
        S.background(surface),
        S.border(Border.solid(1.px, line)),
        S.borderRadius.px(6),
        S.padding(0.75.rem),
        S.margin(0.px),
        S.overflowX.auto,
      )

  object Hint
      extends CssClass(
        S.color(muted),
        S.fontSize.px(12),
        S.lineHeight(1.4),
      )

  def modeButton(mode: Source[String], label: String): UI[Any] =
    E.button(
      mode.map(m => if m == label then Set[CssClass](BtnOn) else Set[CssClass](Btn)),
      Events.onClick(_ => mode.set(label)),
      label,
    )
end DocsUi
