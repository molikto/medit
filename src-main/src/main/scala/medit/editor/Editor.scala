package medit.editor

import medit.draw._
import medit.structure.Language
import medit.input._
import medit.utils.nullable

class Editor(language: Language, data: ujson.Value, save: ujson.Value => Unit) extends Mover {

  // states
  protected val root = Node.create(null, language, language.root, data)
  protected var mode: Mode = null

  def render(canvas: Canvas, width: Int, height: Int): Unit = {
    val timeStart = System.currentTimeMillis()
    root.layout(width, width, false)
    root.measure()
//    if (mode == null) {
//      mode = root.pointedPos(0, 0)
//    }
    val timeLayout = System.currentTimeMillis()
    canvas.save()
    canvas.translate(scrollX.toFloat, scrollY.toFloat)
//    mode match {
//      case Mode.Insert(node, pos, _, small, total) =>
//        val (t, rect) = root.get(node, pos)
//        val pre = t.measurePrefix(small)
//        canvas.draw(Rect(rect.left + pre, rect.top, 3, rect.height), ShapeStyle.cursor)
//      case Mode.Frag(node, pos) =>
//        canvas.draw(root.get(node, pos)._2, ShapeStyle.cursor)
//    }
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
      mode = root.pointedPos((xpos - scrollX).toFloat, (ypos - scrollY).toFloat)
    }
  }

  def onChar(codepoint: Codepoint, mods: Mods): Unit = mode match {
//    case Mode.Frag(node, pos) =>
//      codepoint.toChar match {
//        case 'u' =>
////          if (focus.nonEmpty) focus = focus.dropRight(1)
//        case 'd' =>
////          if (root(focus).childs.nonEmpty) {
////            focus = focus :+ 0
////          }
//        case 'n' =>
////          if (focus.nonEmpty) {
////            val p = root(focus).parent
////            if (p != null && p.childs.size > focus.last + 1) {
////              focus = focus.dropRight(1) :+ (focus.last + 1)
////            }
////          }
//        case 'p' =>
////          if (focus.nonEmpty) {
////            if (focus.last > 0) {
////              focus = focus.dropRight(1) :+ (focus.last - 1)
////            }
////          }
//        case 'D' =>
////          if (focus.nonEmpty) {
////            root(focus.dropRight(1)) match {
////              case col: Node.Collection =>
////                col.tryDelete(focus.last)
////              case _ =>
////            }
////          }
//        case 'i' =>
//          val (tn, _)= root.get(node, pos)
//          if (tn.node != null) {
//            mode = Mode.Insert(node, pos, true, 0, tn.text.size)
//          }
//        case 'j' =>
//          //visualDown(focus).foreach(focus = _)
//        case 'k' =>
//          //visualUp(focus).foreach(focus = _)
//        case 's' =>
//          save(root.save())
//        case _ =>
      //}
    case Mode.Insert(node, pos, editable, small, total) =>
      if (editable) {
        if (codepoint == ' ') {
          root(node).editCommit()
          //mode = Mode.Frag(node, 0)
        } else {
          root(node).editAppend(codepoint, small)
          mode = Mode.Insert(node, pos, editable, small + 1, total + 1)
        }
      }
  }

  def onKey(key: Int, action: Int, mods: Mods): Unit = {
    // TODO implement repeat keys
    if (action == 1) {
      mode match {
        case Mode.Insert(node, pos, editable, small, total) =>
          if (key == Key.Backspace) {
            if (editable) {
              if (small > 0) {
                root(node).editBackspace(small)
                mode = Mode.Insert(node, pos, editable, small - 1, total - 1)
              }
            }
          } else if (key == Key.Left) {
            if (small > 0) {
              mode = Mode.Insert(node, pos, editable, small - 1, total)
            }
          } else if (key == Key.Right) {
            if (small < total) {
              mode = Mode.Insert(node, pos, editable, small + 1, total)
            }
          }
      }
    }
  }
}
