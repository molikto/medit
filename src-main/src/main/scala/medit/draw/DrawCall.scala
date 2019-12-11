package medit.draw

case class Size(height: Float, width: Float)

case class TextMeasure(my: Float, y: Float, w: Float) {
  def *(a: Int) = TextMeasure(my, y, w * a)
  def +(y: TextMeasure): TextMeasure = TextMeasure(my max y.my, this.y max y.y, w + y.w)
}

case class Position(top: Float, left: Float, depth: Int) {
  def +(p: Position) = Position(top + p.top, left + p.left, depth + p.depth)
}

object Position {
  val unit = Position(0, 0, 0)
}
case class Rect(top: Float, left: Float, height: Float, width: Float) {
  def +(p: Position) = Rect(top + p.top, left + p.left, height, width)
}

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
  val cursor : ShapeStyle = ShapeStyle(0xFF003300)
  val error: ShapeStyle = ShapeStyle(0xFFAA0000)
}

sealed trait DrawCall {
  def translated(position: Position): DrawCall = if (position == Position.unit) this else this match {
    case DrawCall.Translated(p, calls) =>
      DrawCall.Translated(p + position, calls)
    case DrawCall.Group(calls) =>
      DrawCall.Translated(position, calls)
    case _ =>
      DrawCall.Translated(position, Seq(this))
  }

}

object DrawCall {
  case class Text(position: Position, style: TextStyle, text: Str) extends DrawCall
  case class Rect(rect: medit.draw.Rect, style: ShapeStyle) extends DrawCall
  case class Translated(position: Position, calls: Seq[DrawCall]) extends DrawCall
  case class Group(calls: Seq[DrawCall]) extends DrawCall
}
