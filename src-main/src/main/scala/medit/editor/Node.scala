package medit.editor

import medit.draw
import medit.draw.DrawCall
import medit.structure.{Language, Layout}

import scala.collection.mutable

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {
  private var _parent: Node = null // root node has empty parent
  private var _layout: Layout = null
  private var _draw: DrawCall = null
  def layout: Layout = ???

  def draw(width: Int): DrawCall = _draw
}

object Node {

  def root(a: Language): Node = {
    new Structure(a, a.sorts.size - 1, -1)
  }

  // structure0 index = -1 when single sort
  class Structure(val language: Language, val sort0: Int, val structure0: Int) extends Node {
    private val childs = new mutable.ArrayBuffer[Node]()
  }

  class Star(val language: Language, val sort0: Int) extends Node {
    private val childs = new mutable.ArrayBuffer[Structure]()
  }

  class Str() extends Node
}
