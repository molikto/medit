package medit.draw

case class Position(top: Int, left: Int, depth: Int)

sealed trait Typeface
object Typeface {
  case object Mono extends Typeface
}

case class TextStyle(color: Int, typeface: Typeface, size: Int) {
  def measure(text: Str) = impl.measure(this, text)
}
object TextStyle {
}

sealed trait DrawCall {
  val position: Position
}

object DrawCall {
  case class Text(override val position: Position, style: TextStyle, text: Str) extends DrawCall
  case class Translated(position: Position, calls: Seq[DrawCall]) extends DrawCall
}
