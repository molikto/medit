package medit.editor

import medit.draw._
import medit.structure.Language
import medit.input._

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
    lines.render(canvas)
    mode match {
      case Mode.Insert(index) =>
        val rect = lines.get(index).rect
        canvas.draw(Rect(rect.left, rect.top, rect.width + 3, rect.height), ShapeStyle.cursor)
      case Mode.DeleteConfirm(node, prev) =>
        // FIXME draw this
    }
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

  def onChar(codepoint: Codepoint, mods: Mods): Unit = {
    mode match {
      case Mode.Insert(a) =>
        val texts = lines.get(a).pos
        def insertChar(node: Node.EditableLeaf, pos: Int): Unit = {
          node.appendEdit(codepoint, pos)
          mode = Mode.Insert(a + 1)
        }
        texts match {
          case PosInfo.MiddleOf(left, right) =>
            var done = false
            if (left != null) {
              left.resolve() match {
                case JustStringNode(node) =>
                  if (codepoint == ' ') {
                    node match {
                      case choice: Node.Choice =>
                        choice.tryCommit()
                        done = true
                      case _ =>
                    }
                  } else {
                    done = true
                    insertChar(node, node.text.size)
                  }
                case _ =>
              }
            }
            if (!done && right != null) {
              right.resolve() match {
                case JustStringNode(node) =>
                  done = true
                  insertChar(node, 0)
                case _ =>
              }
            }
          case PosInfo.Inside(text, pos) =>
            text.resolve() match {
              case JustStringNode(node) =>
                insertChar(node, pos)
              case _ =>
            }
        }
    }
  }

  def onKey(key: Int, action: Int, mods: Mods): Unit = {
    // TODO implement repeat keys
    if (action == 1) {
      mode match {
        case Mode.Insert(a) =>
          val texts = lines.get(a).pos
          key match {
            case Key.Backspace =>
              val lefty = texts.lefty
              if (lefty != null) {
                lefty._1.resolve() match {
                  case JustStringNode(node) =>
                    node.backspaceEdit(lefty._2)
                    mode = Mode.Insert(a - 1)
                  case a =>
                    if (a.lastOfNode) {
                      mode = Mode.DeleteConfirm(a.node.path, mode)
                    }
                }
              }
            case Key.Enter =>
              texts match {
                case PosInfo.MiddleOf(left, right) =>
                  if (left != null) {
                    val rv = left.resolve()
                    rv match {
                      case PartOfCollection.B1(node) =>
                        node.insertNewAt(0)
                      case PartOfCollection.SeparatorAt(node, index) =>
                        node.insertNewAt(index)
                      case _ =>
                        if (rv.lastOfNode) {
                          rv.node.parent match {
                            case node: Node.Collection =>
                              node.insertNewAt(node.childs.indexOf(rv.node) + 1)
                            case _ =>
                          }
                        } else {

                        }
                    }
                  }
                case _ =>
              }
            case _ =>
          }
        case Mode.DeleteConfirm(node, Mode.Insert(index)) =>
          key match {
            case Key.Backspace =>
              val n = root(node)
              if (n.remove()) {
                mode = Mode.Insert(index - n.frag.size)
              }
            case _ =>
          }
      }
    }
  }
}
