package medit.draw

import medit.utils._

case class Size(width: Float, height: Float)
object Size {
  val unit = Size(0, 0)

  val sumHeight = new Numeric[Size] {
    override def plus(x: Size, y: Size): Size = Size(x.width max y.width, x.height + y.height)
    override def fromInt(x: Int): Size = Size(x, 0)
    override def minus(x: Size, y: Size): Size = notUsed()
    override def times(x: Size, y: Size): Size = notUsed()
    override def negate(x: Size): Size = notUsed()
    override def parseString(str: String): Option[Size] = notUsed()
    override def toInt(x: Size): Int = notUsed()
    override def toLong(x: Size): Long = notUsed()
    override def toFloat(x: Size): Float = notUsed()
    override def toDouble(x: Size): Double = notUsed()
    override def compare(x: Size, y: Size): Int = notUsed()
  }
  val sumWidth = new Numeric[Size] {
    override def plus(x: Size, y: Size): Size = Size(x.width + y.width, x.height max y.height)
    override def fromInt(x: Int): Size = Size(x, 0)
    override def minus(x: Size, y: Size): Size = notUsed()
    override def times(x: Size, y: Size): Size = notUsed()
    override def negate(x: Size): Size = notUsed()
    override def parseString(str: String): Option[Size] = notUsed()
    override def toInt(x: Size): Int = notUsed()
    override def toLong(x: Size): Long = notUsed()
    override def toFloat(x: Size): Float = notUsed()
    override def toDouble(x: Size): Double = notUsed()
    override def compare(x: Size, y: Size): Int = notUsed()
  }
}

case class TextMeasure(w: Float, my: Float, y: Float) {
  def *(a: Int) = TextMeasure(w * a, my, y)
  def +(y: TextMeasure): TextMeasure = TextMeasure(w + y.w, my max y.my, this.y max y.y)
}

case class Position(left: Float, top: Float, depth: Int) {
  def +(p: Position) = Position(left + p.left, top + p.top, depth + p.depth)
}

object Position {
  val unit = Position(0, 0, 0)
}
case class Rect(left: Float, top: Float, height: Float, width: Float) {
  def contains(xpos: Float, ypos: Float): Boolean = {
    xpos >= left && xpos <= left + width && ypos >= top && ypos <= top + height
  }

  def +(p: Position) = Rect(left + p.left, top + p.top, height, width)
}

sealed trait Typeface
object Typeface {
  case object Mono extends Typeface
}

case class TextStyle(color: Int, typeface: Typeface, size: Int) {
  def measure(text: String) = impl.measure(this, text)
  lazy val unit = measure(" ")
}


object TextStyle {
  val keyword = TextStyle(0xFFCC7832, Typeface.Mono, 13)
  val reference = TextStyle(0xFFA9B7C6, Typeface.Mono, 13)
  val delimiters = TextStyle(0xFF787878, Typeface.Mono, 13)
  val error = TextStyle(0xFFAA0000, Typeface.Mono, 13)
}


case class ShapeStyle(color: Int)

object ShapeStyle {
  val cursor : ShapeStyle = ShapeStyle(0x99214283)
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
  case class Text(position: Position, style: TextStyle, text: String) extends DrawCall
  case class Rect(rect: medit.draw.Rect, style: ShapeStyle) extends DrawCall
  case class Translated(position: Position, calls: Seq[DrawCall]) extends DrawCall
  case class Group(calls: Seq[DrawCall]) extends DrawCall
}
