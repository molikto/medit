package medit.editor

import medit.draw._
import medit.structure.Language
import medit.input._

class Editor(language: Language) {

  // TODO load from disk format
  val root = Node.root(language)

  def render(width: Int): DrawCall = root.draw(width)

  def onChar(codepoint: Codepoint, mods: Mods): Unit = {
  }

  def onKey(key: Int, mods: Mods): Unit = {
  }
}
