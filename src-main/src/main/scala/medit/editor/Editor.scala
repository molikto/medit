package medit.editor

import medit.draw._
import medit.structure.Language
import medit.input._
import medit.utils.nullable

class Editor(language: Language, data: ujson.Value, save: ujson.Value => Unit) extends Mover {


  // states
  protected val root = Node.create(null, language, language.root, data)
  private var lines: Page = null
  protected var mode: Mode = null

  def render(canvas: Canvas, width: Int, height: Int): Unit = {
    val timeStart = System.currentTimeMillis()
    root.layout(width, width, false)
    lines = new Page()
    root.frag.finish(lines)
    lines.break()
    if (mode == null) {
      mode = Mode.Insert(Index(0, true))
    }
    val timeLayout = System.currentTimeMillis()
    canvas.save()
    canvas.translate(scrollX.toFloat, scrollY.toFloat)
    mode match {
      case Mode.Insert(index) =>
        val rect = lines.get(index).rect
        canvas.draw(Rect(rect.left, rect.top, 3, rect.height), ShapeStyle.cursor)
    }
    lines.render(canvas)
    canvas.restore()
    val timeDraw = System.currentTimeMillis()

    val layoutStr = s"${timeLayout - timeStart} layout"
    val layoutMeasure = TextStyle.delimiters.measure(layoutStr)
    canvas.draw(layoutStr, TextStyle.delimiters, width - layoutMeasure.width, layoutMeasure.y)

    val drawStr = s"${timeDraw - timeLayout} draw"
    val drawMeasure = TextStyle.delimiters.measure(drawStr)
    canvas.draw(drawStr, TextStyle.delimiters, width - drawMeasure.width, drawMeasure.y + layoutMeasure.y + layoutMeasure.my)
  }

  var scrollX = 0.0
  var scrollY = 0.0

  // TODO why this scroll is so small compared with other app??
  def onScroll(xoffset: Double, yoffset: Double): Unit = {
    //scrollX += xoffset
    scrollY += yoffset
  }

  // left 0, right 1, middle 2
  // press 1, release 0
  def onMouse(button: Int, press: Int, mods: Mods, xpos: Double, ypos: Double): Unit = {
    if (button == 0 && press == 1) {
      mode = Mode.Insert(lines.reverse((xpos - scrollX).toFloat, (ypos - scrollY).toFloat))
    }
  }

  def onChar(codepoint: Codepoint, mods: Mods): Unit = mode match {
    case Mode.Insert(a) =>
      val texts = lines.get(a).pos
      if (codepoint == ' ') {
      } else {
        if (texts.exists(p => p.text.editable && p.pos > 0 && p.pos < p.text.size)) {
          assert(texts.size == 1)
          val p = texts.head
          p.text.node.appendEdit(codepoint, p.pos)
          mode = Mode.Insert(a.inc)
        }
      }
  }

  def onKey(key: Int, action: Int, mods: Mods): Unit = {
    // TODO implement repeat keys
    if (action == 1) {
      mode match {
        case Mode.Insert(a) =>
          if (key == Key.Backspace) {
          }
      }
    }
  }
}
