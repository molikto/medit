package medit.draw

case class Position(top: Int, left: Int, depth: Int)

case class Rect(top: Int, left: Int, width: Int, height: Int)

sealed trait Typeface
object Typeface {
  case object Mono extends Typeface
}

case class TextStyle(color: Int, typeface: Typeface, size: Int) {
  def measure(text: Str) = impl.measure(this, text)
}


object TextStyle {
  val default = TextStyle(0xFFFFFFFF, Typeface.Mono, 12)
}


case class ShapeStyle(color: Int)

object ShapeStyle {
  def default: ShapeStyle = ShapeStyle(0xFFAA0000)
}

sealed trait DrawCall

object DrawCall {
  case class Text(position: Position, style: TextStyle, text: Str) extends DrawCall
  case class Rect(rect: medit.draw.Rect, style: ShapeStyle) extends DrawCall
  case class Translated(position: Position, calls: Seq[DrawCall]) extends DrawCall
  case class Group(calls: Seq[DrawCall]) extends DrawCall
}
