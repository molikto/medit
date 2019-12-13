package medit.editor

import medit.draw
import medit.draw.{Canvas, Position, Rect, Size, TextMeasure, TextStyle}
import medit.utils._

import scala.collection.mutable


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

  def get(index: Int): (LineFrag.Text, Rect) = {
    this match {
      case frag: LineFrag.Text =>
        if (index == 0) (frag, Rect(0, 0, size.width, size.height))
        else logicError()
      case _ =>
        var i = 0
        var c = 0
        var res: (LineFrag.Text, Rect) = null
        while (i < frags.size && res == null) {
          val f = frags(i)
          if (c + f.count > index) {
            val rr = f.get(index - c)
            res = (rr._1, rr._2 + Position(f.left, f.top))
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

  def pointedLine(xpos: Float, ypos: Float): (LineFrag, Float) = {
    this match {
      case block: Block =>
        // TODO what if there is no document?
        val b = block.frags.find(a => a.top + a.size.height > ypos).getOrElse(block.frags.last)
        b.pointedLine(xpos - b.left, ypos - b.top)
      case l: LineFrag =>
        (l, xpos)
    }
  }

  def pointedPos(xpos: Float, ypos: Float): (LineFrag.Text, Int) = {
    val (l, x) = pointedLine(xpos, ypos)
    l.pointedPos(x)
  }

  def pointedText(xpos: Float, ypos: Float): LineFrag.Text = {
    val (l, x) = pointedLine(xpos, ypos)
    l.pointedText(x)
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

  def flattenAtomics(xpos: Float): Seq[(LineFrag.Atomic, Float)] = this match {
    case text: LineFrag.Text => Seq((text, xpos))
    case p: LineFrag.Pad => Seq((p, xpos))
    case compose: LineFrag.Compose => compose.frags.flatMap(a => a.flattenAtomics(xpos + a.left))
  }

  def flattenTexts(xpos: Float): Seq[(LineFrag.Text, Float, Boolean)] = {
    val res = flattenAtomics(xpos)
    val a = mutable.ArrayBuffer[(LineFrag.Text, Float, Boolean)]()
    for (r <- res) {
      r._1 match {
        case text: LineFrag.Text =>
          a.append((text, r._2, false))
        case pad: LineFrag.Pad =>
          if (a.nonEmpty) {
            a.update(a.size - 1, a(a.size - 1).copy(_3 = true))
          }
      }
    }
    a.toSeq
  }

  // prefer a editable cell
  private def pointedText0(xpos: Float): (LineFrag.Text, LineFrag.Text, Float, LineFrag.Text) = {
    val tokens = flattenTexts(0)
    var index = tokens.indexWhere(a => a._2 <= xpos && a._2 + a._1.size.width > xpos)
    if (index < 0) {
      index = tokens.zipWithIndex.map(a0 => {
        val a = a0._1
        val dis = Math.abs(a._2 - xpos) min Math.abs(a._1.size.width + a._2 - xpos)
        (dis, a0._2)
      }).minBy(_._1)._2
    }
    val tk = tokens(index)
    (if (index == 0 || tokens(index - 1)._3) null else tokens(index - 1)._1,
        tk._1, tk._2,
        if (index == tokens.size - 1 || tk._3) null else tokens(index + 1)._1)
  }

  def pointedPos(xpos: Float): (LineFrag.Text, Int) = {
    val (before, t, x, after) = pointedText0(xpos)
    val pos = if (xpos <= x) {
      0
    } else if (x + t.size.width < xpos) {
      t.text.length
    } else {
      var res = -1
      var i = 1
      var pdiff = xpos - x
      while (i < t.text.length && res == -1) {
        val diff = xpos - x - t.measurePrefix(i)
        if (diff == 0) {
          res = i
        } else if (diff < 0) {
          if (pdiff > -diff) {
            res = i
          } else {
            res = i - 1
          }
        } else {
          pdiff = diff
        }
        i += 1
      }
      if (res == -1) {
        if (Math.abs(xpos - x - t.size.width) < pdiff) {
          res = t.text.length
        } else {
          res = t.text.length - 1
        }
      }
      res
    }
    var res = (t, pos)
    if (pos == 0) {
      val beforeEditable = before != null && before.editable
      if (t.editable) {
        if (beforeEditable) warn("This is a ambiguous insertion point")
      } else {
        if (beforeEditable) res = (before, before.text.size)
      }
    } else if (pos == t.text.size) {
      val afterEditable = after != null && after.editable
      if (t.editable) {
        if (afterEditable) warn("This is a ambiguous insertion point")
      } else {
        if (afterEditable) res = (after, 0)
      }
    }
    res
  }

  def pointedText(xpos: Float): LineFrag.Text = {
    pointedText0(xpos)._2
  }
}
object LineFrag {

  sealed trait Atomic extends LineFrag

  class Text(val text: String, val style: TextStyle, pad: Float = 0) extends Atomic {
    def editable: Boolean = node != null && node.editable

    // TODO use codepoint index, or even better grapheme index
    def measurePrefix(small: Int): Float =
      if (small == 0) 0
      else if (small == text.size) measure.width
      else style.measure(text.take(small)).width + pad
    val measure = {
      val res = style.measure(text)
      if (pad == 0) {
        res
      } else {
        res.copy(width = res.width + pad * 2)
      }
    }
    def count = 1
    def width = measure.width

    def parentNodeWithRelativeIndex(): (Node, Int) = {
      parentNodeWithRelativeIndexInner()
    }

    override def render(canvas: Canvas): Unit =
      canvas.draw(text, style, pad, measure.y)
  }

  class Pad(val width: Float) extends Atomic {
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
