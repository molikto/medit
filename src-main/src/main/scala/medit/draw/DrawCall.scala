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
  val keyword = TextStyle(0xFFFFFFAA, Typeface.Mono, 13)
  val reference = TextStyle(0xFFFFFFFF, Typeface.Mono, 13)
  val delimiters = TextStyle(0xFFAAAAAA, Typeface.Mono, 13)
  val error = TextStyle(0xFFAA0000, Typeface.Mono, 13)
}


case class ShapeStyle(color: Int)

object ShapeStyle {
  val cursor : ShapeStyle = ShapeStyle(0xFF00AA00)
  val error: ShapeStyle = ShapeStyle(0xFFAA0000)
}

sealed trait DrawCall

object DrawCall {
  case class Text(position: Position, style: TextStyle, text: Str) extends DrawCall
  case class Rect(rect: medit.draw.Rect, style: ShapeStyle) extends DrawCall
  case class Translated(position: Position, calls: Seq[DrawCall]) extends DrawCall
  case class Group(calls: Seq[DrawCall]) extends DrawCall
}
