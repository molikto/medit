package medit.editor

import medit.draw._
import medit.structure.{Data, Language}
import medit.input._
import medit.utils.nullable

import scala.collection.mutable.ArrayBuffer

class Editor(language: Language, data: ujson.Value, save: ujson.Value => Unit) extends Mover {

  // states
  protected val root = Node.create(null, language, language.root, data)
  protected var focus: Seq[Int] = Seq.empty
  @nullable var editMode: Node = null

  def render(width: Int): DrawCall = {
    root.layout(width)
    var rect = root.rect(focus)
    if (editMode != null) rect = rect.copy(width = 1)
    DrawCall.Group(Seq(DrawCall.Rect(rect, ShapeStyle.cursor), root.draw))
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
