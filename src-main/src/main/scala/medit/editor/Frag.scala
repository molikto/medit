package medit.editor

import medit.draw
import medit.draw.{Canvas, Position, Rect, Size, TextMeasure, TextStyle}
import medit.utils._


sealed trait Frag {


  def frags: Seq[Frag] = Seq.empty

  def render(canvas: Canvas): Unit

  var parent: Frag = null
  var left: Float = 0
  var top: Float = 0
  def count: Int
  def size: Size

  @nullable var node: Node = null // if a node is represented by this frag

  @nullable def fragEnclosing(xpos: Float, ypos: Float): Frag = {
    if (xpos >= 0 && ypos >= 0 && xpos <= size.width && ypos <= size.height) {
      var i = 0
      var res: Frag = null
      while (i < frags.size && res == null) {
        val f = frags(i)
        res = f.fragEnclosing(xpos - f.left, ypos - f.top)
        i += 1
      }
      if (res == null) res = this
      res
    } else {
      null
    }
  }

  def rect(index: Int): Rect = {
    this match {
      case frag: LineFrag.Text =>
        if (index == 0) Rect(0, 0, size.width, size.height)
        else logicError()
      case _ =>
        var i = 0
        var c = 0
        var res: Rect = null
        while (i < frags.size && res == null) {
          val f = frags(i)
          if (c + f.count > index) {
            res = f.rect(index - c) + Position(f.left, f.top)
          }
          c += f.count
          i += 1
        }
        if (res == null) {
          logicError()
        }
        res
    }
  }

  def pointedText(xpos: Float, ypos: Float): LineFrag.Text = {
    this match {
      case block: Block =>
        // TODO what if there is no document?
        val b = block.frags.find(a => a.top + a.size.height > ypos).getOrElse(block.frags.last)
        b.pointedText(xpos - b.left, ypos - b.top)
      case l: LineFrag =>
        l.pointedText(xpos)
    }
  }

  def parentNode(): Node = {
    if (this.node == null) {
      this.parentNode()
    } else {
      node
    }
  }



  protected def parentNodeWithRelativeIndexInner(): (Node, Int) = {
    if (this.node == null) {
      val p = parent
      val index = p.frags.indexOf(this)
      val before = p.frags.take(index).map(_.count).sum
      val (n, b2) = p.parentNodeWithRelativeIndexInner()
      (n, before + b2)
    } else {
      (node, 0)
    }
  }
  //  def reverse(a: LineFrag.Text): (Node, Int) = {
//
//  }
}

sealed trait HasChildFrag extends Frag {
  def render(canvas: Canvas): Unit = {
    for (f <- frags) {
      canvas.save()
      canvas.translate(f.left, f.top)
      f.render(canvas)
      canvas.restore()
    }
  }

  lazy val count: Int = frags.map(_.count).sum
}

sealed trait LineFrag extends Frag {
  def measure: TextMeasure
  def width: Float
  def size = Size(measure.width, measure.y + measure.my)

  def flattenTexts(xpos: Float): Seq[(LineFrag.Text, Float)] = this match {
    case text: LineFrag.Text => Seq((text, xpos))
    case _: LineFrag.Pad => Seq.empty
    case compose: LineFrag.Compose => compose.frags.flatMap(a => a.flattenTexts(xpos + a.left))
  }

  def pointedText(xpos: Float): LineFrag.Text = {
    val tokens = flattenTexts(0)
    tokens.find(a => a._2 <= xpos && a._2 + a._1.size.width > xpos) match {
      case Some(value) => value._1
      case None =>
        tokens.map(a => {
         val dis = Math.abs(a._2 - xpos) min Math.abs(a._1.size.width + a._2 - xpos)
          (dis, a._1)
        }).minBy(_._1)._2
    }
  }
}
object LineFrag {

  class Text(val text: String, val style: TextStyle) extends LineFrag {
    val measure = style.measure(text)
    def count = 1
    def width = measure.width

    def parentNodeWithRelativeIndex(): (Node, Int) = {
      parentNodeWithRelativeIndexInner()
    }

    override def render(canvas: Canvas): Unit =
      canvas.draw(text, style, 0, measure.y)

  }
  class Pad(val width: Float) extends LineFrag {
    val measure: TextMeasure = TextMeasure(width, 0, 0)
    override def render(canvas: Canvas): Unit = {}
    def count = 0
  }
  class Compose(override val frags: Seq[LineFrag]) extends LineFrag with HasChildFrag {
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
  }
}

//class Compose(before: LineFrag, center: Frag, after: LineFrag) extends Frag {
//
//}

class Block(val pad: Float, override val frags: Seq[Frag]) extends Frag with HasChildFrag {
  lazy val size: Size = if (frags.isEmpty) Size.unit else {
    var height = 0F
    var width = 0F
    var i = 0
    while (i < frags.size) {
      val frag = frags(i)
      frag.parent = this
      frag.left = pad
      width = frag.size.width max width
      frag.top = height
      height += frag.size.height
      i += 1
    }
    Size(width + pad, height)
  }
}
