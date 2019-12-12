package medit.editor

import medit.draw.{Position, Size}


sealed trait DrawTemplate {
  def translated(position: Position): DrawTemplate = if (position == Position.unit) this else this match {
    case DrawTemplate.Translated(p, calls) =>
      DrawTemplate.Translated(p + position, calls)
    case DrawTemplate.Group(calls) =>
      DrawTemplate.Translated(position, calls)
    case _ =>
      DrawTemplate.Translated(position, Seq(this))
  }
}
object DrawTemplate {
  case class Child(i: Int) extends DrawTemplate
  case class ChildChild(i: Int, j: Int) extends DrawTemplate
  case class Translated(position: Position, temps: Seq[DrawTemplate]) extends DrawTemplate
  case class Group(temps: Seq[DrawTemplate]) extends DrawTemplate
  case class Just(call: medit.draw.DrawCall) extends DrawTemplate
}
