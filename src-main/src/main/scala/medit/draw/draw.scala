package medit.draw

import medit.utils._


trait Impl {
  def measure(textStyle: TextStyle, str: String): TextMeasure
}
trait Canvas {
  def draw(text: String, style: TextStyle, left: Float, top: Float): Unit
  def draw(rect: Rect, style: ShapeStyle): Unit
  def save(): Unit
  def restore(): Unit
  def translate(x: Float, y: Float): Unit
}

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

case class TextMeasure(width: Float, my: Float, y: Float) {
  def *(a: Int) = TextMeasure(width * a, my, y)
  def +(y: TextMeasure): TextMeasure = TextMeasure(width + y.width, my max y.my, this.y max y.y)
}
object TextMeasure {
  val empty = TextMeasure(0, 0, 0)
}

case class Position(left: Float, top: Float) {
  def +(p: Position) = Position(left + p.left, top + p.top)
}

object Position {
  val unit = Position(0, 0)
}
case class Rect(left: Float, top: Float, width: Float, height: Float) {
  def leftTop: Position = Position(left, top)


  def contains(xpos: Float, ypos: Float): Boolean = {
    xpos >= left && xpos <= left + width && ypos >= top && ypos <= top + height
  }

  def +(p: Position) = Rect(left + p.left, top + p.top, width, height)
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
  def find(A: String): Option[TextStyle] = A match {
    case "keyword" => Some(keyword)
    case "reference" => Some(reference)
    case "declare" => Some(declare)
    case "const" => Some(const)
    case _ => None
  }

  val keyword = TextStyle(0xFFCC7832, Typeface.Mono, 13)
  val choice = TextStyle(0xFFFFC66D, Typeface.Mono, 13)
  val reference = TextStyle(0xFFb9acbf, Typeface.Mono, 13)
  val declare = TextStyle(0xFFA9B7C6, Typeface.Mono, 13)
  val const = TextStyle(0xFF6A8759, Typeface.Mono, 13)
  val delimiters = TextStyle(0xFF787878, Typeface.Mono, 3)
  val error = TextStyle(0xFFAA0000, Typeface.Mono, 13)
}


case class ShapeStyle(color: Int)

object ShapeStyle {
  val cursor : ShapeStyle = ShapeStyle(0xFF214283)
  val selection: ShapeStyle = ShapeStyle(0x99214283)
  val error: ShapeStyle = ShapeStyle(0xFFAA0000)
}
