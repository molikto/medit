package medit.editor

import medit.draw
import medit.draw.{Canvas, Rect, Size, TextMeasure, TextStyle}
import medit.utils.nullable

sealed trait Frag {
  def render(canvas: Canvas): Unit

  var parent: Frag = null
  var left: Float = 0
  var top: Float = 0
  def size: Size

  @nullable var node: Node = null // if a node is represented by this frag
  def wrap(): Block = this match {
    case frag: LineFrag => new Block.Line(frag)
    case block: Block => block
  }
}
object Frag {
  def renderAll(canvas: Canvas, frags: Seq[Frag]): Unit = {
    for (f <- frags) {
      canvas.save()
      canvas.translate(f.left, f.top)
      f.render(canvas)
      canvas.restore()
    }
  }
}

sealed trait LineFrag extends Frag {
  def measure: TextMeasure
  def width: Float
  def size = Size(measure.width, measure.y + measure.my)
}
object LineFrag {
  class Text(val text: String, val style: TextStyle) extends LineFrag {
    val measure = style.measure(text)
    def width = measure.width

    override def render(canvas: Canvas): Unit =
      canvas.draw(text, style, 0, measure.y)
  }
  class Pad(val width: Float) extends LineFrag {
    val measure: TextMeasure = TextMeasure(width, 0, 0)
    override def render(canvas: Canvas): Unit = {}
  }
  class Compose(val frags: Seq[LineFrag]) extends LineFrag {
    val width: Float = frags.map(_.width).sum

    lazy val measure: TextMeasure = {
      var y = 0F
      var my = 0F
      var i = 0
      while (i < frags.size) {
        val frag = frags(i)
        frag.parent = this
        y = y max frag.measure.y
        my = my max frag.measure.my
        i += 1
      }
      var width = 0F
      i = 0
      while (i < frags.size) {
        val frag = frags(i)
        frag.left = width
        width += frag.measure.width
        frag.top = y - frag.measure.y
        i += 1
      }
      TextMeasure(width, my, y)
    }
    override def render(canvas: Canvas): Unit = Frag.renderAll(canvas, frags)
  }
}

sealed trait Block extends Frag
object Block {
  class Line(val line: LineFrag) extends Block {
    lazy val size: Size = {
      line.parent = this
      line.size
    }

    override def render(canvas: Canvas): Unit = line.render(canvas)
  }
  class Compose(val pad: Float, val blocks: Seq[Block]) extends Block {
    lazy val size: Size = if (blocks.isEmpty) Size.unit else {
      var height = 0F
      var width = 0F
      var i = 0
      while (i < blocks.size) {
        val frag = blocks(i)
        frag.parent = this
        frag.left = pad
        width = frag.size.width max width
        frag.top = height
        height += frag.size.height
        i += 1
      }
      Size(width + pad, height)
    }

    override def render(canvas: Canvas): Unit = Frag.renderAll(canvas, blocks)
  }
}


