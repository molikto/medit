package medit.editor

import medit.draw._
import medit.structure.{Data, Language}
import medit.input._
import medit.utils.nullable

class Editor(language: Language, data: ujson.Value, save: ujson.Value => Unit) extends Mover {

  // states
  protected val root = Node.create(null, language, language.root, data)
  protected var focus: Seq[Int] = Seq(0)
  @nullable var editMode: Node = null

  def render(canvas: Canvas, width: Int, height: Int): Unit = {
    val timeStart = System.currentTimeMillis()
    root.layout(width, false)
    val hack = root.frag.size
    val timeLayout = System.currentTimeMillis()
    canvas.save()
    canvas.translate(scrollX.toFloat, scrollY.toFloat)
    canvas.draw(root.rect(focus), ShapeStyle.cursor)
    root.render(canvas)
    canvas.restore()
    val timeDraw = System.currentTimeMillis()

    val layoutStr = s"${timeLayout - timeStart} layout time"
    val layoutMeasure = TextStyle.delimiters.measure(layoutStr)
    canvas.draw(layoutStr, TextStyle.delimiters, width - layoutMeasure.width, layoutMeasure.y)

    val drawStr = s"${timeDraw - timeLayout} draw time"
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
      val r = root.reverse((xpos - scrollX).toFloat, (ypos - scrollY).toFloat)
      if (r != null) focus = r
    }
  }

  def onChar(codepoint: Codepoint, mods: Mods): Unit = if (editMode == null) {
    codepoint.toChar match {
      case 'u' =>
        if (focus.nonEmpty) focus = focus.dropRight(1)
      case 'd' =>
        if (root(focus).childs.nonEmpty) {
          focus = focus :+ 0
        }
      case 'n' =>
        if (focus.nonEmpty) {
          val p = root(focus).parent
          if (p != null && p.childs.size > focus.last + 1) {
            focus = focus.dropRight(1) :+ (focus.last + 1)
          }
        }
      case 'p' =>
        if (focus.nonEmpty) {
          if (focus.last > 0) {
            focus = focus.dropRight(1) :+ (focus.last - 1)
          }
        }
      case 'D' =>
        if (focus.nonEmpty) {
          root(focus.dropRight(1)) match {
            case col: Node.Collection =>
              col.tryDelete(focus.last)
            case _ =>
          }
        }
      case 'i' =>
        val node = root(focus)
        if (node.tryEdit()) {
          editMode = node
        }
      case 'j' =>
        visualDown(focus).foreach(focus = _)
      case 'k' =>
        visualUp(focus).foreach(focus = _)
      case 'a' =>
        val index = root(focus).tryNewChild()
        if (index >= 0) {
          focus = focus :+ index
        }
      case 's' =>
        save(root.save())
      case _ =>
    }
  } else {
    if (codepoint == ' ') {
      editMode.editCommit()
      editMode = null
    } else {
      editMode.editAppend(codepoint)
    }
  }

  def onKey(key: Int, mods: Mods): Unit = {
    if (editMode != null) {
      if (key == Key.Backspace) {
        editMode.editBackspace()
      }
    } else {

    }
  }
}
