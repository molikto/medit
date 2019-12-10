package medit.editor

import medit.draw._
import medit.structure.Language
import medit.input._
import medit.utils.nullable

import scala.collection.mutable.ArrayBuffer

class Editor(language: Language) {

  // states
  val root = Node.root(language)
  var focus: Seq[Int] = Seq.empty
  @nullable var editMode: Node = null

  def render(width: Int): DrawCall = {
    root.drawTree(width)
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
      case 'a' =>
        val index = root(focus).tryNewChild()
        if (index >= 0) {
          focus = focus :+ index
        }
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
  }
}
