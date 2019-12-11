package medit.draw

case class Size(height: Float, width: Float)

case class TextMeasure(my: Float, y: Float, w: Float) {
  def *(a: Int) = TextMeasure(my, y, w * a)
  def +(y: TextMeasure): TextMeasure = TextMeasure(my max y.my, this.y max y.y, w + y.w)
}

object TextMeasure {
  implicit val  num: Numeric[TextMeasure] = new Numeric[TextMeasure]() {
    override def plus(x: TextMeasure, y: TextMeasure): TextMeasure = x + y
    override def fromInt(x: Int): TextMeasure = TextMeasure(0, 0, x)

    override def minus(x: TextMeasure, y: TextMeasure): TextMeasure = ???
    override def times(x: TextMeasure, y: TextMeasure): TextMeasure = ???
    override def negate(x: TextMeasure): TextMeasure = ???
    override def parseString(str: String): Option[TextMeasure] = ???
    override def toInt(x: TextMeasure): Int = ???
    override def toLong(x: TextMeasure): Long = ???
    override def toFloat(x: TextMeasure): Float = ???
    override def toDouble(x: TextMeasure): Double = ???
    override def compare(x: TextMeasure, y: TextMeasure): Int = ???
  }
}


case class Position(top: Float, left: Float, depth: Int) {
  def +(p: Position) = Position(top + p.top, left + p.left, depth + p.depth)
}

object Position {
  val unit = Position(0, 0, 0)
}
case class Rect(top: Float, left: Float, height: Float, width: Float)

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
