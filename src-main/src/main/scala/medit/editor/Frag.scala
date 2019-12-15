package medit.editor

import medit.draw.{Canvas, Position, Rect, Size, TextMeasure, TextStyle}
import medit.utils._

import scala.collection.mutable


sealed trait Frag {


  def frags: Seq[Frag]
  def count: Int

  def render(canvas: Canvas): Unit

  var parent: Frag = null
  var left: Float = 0
  var top: Float = 0
  def width: Float
  def height: Float

  final def measure(before: TextMeasure): Unit = {
    doMeasure(before)
  }
  protected def doMeasure(before: TextMeasure): Unit

  @nullable var node: Node = null // if a node is represented by this frag

  @nullable def fragEnclosing(xpos: Float, ypos: Float): Frag = {
//    if (xpos >= 0 && ypos >= 0 && xpos <= size.width && ypos <= size.height) {
//      var i = 0
//      var res: Frag = null
//      while (i < frags.size && res == null) {
//        val f = frags(i)
//        res = f.fragEnclosing(xpos - f.left, ypos - f.top)
//        i += 1
//      }
//      if (res == null) res = this
//      res
//    } else {
//      null
//    }
    ???
  }

  def get(index: Int): (LineFrag.Text, Rect) = {
    this match {
      case frag: LineFrag.Text =>
        if (index == 0) (frag, Rect(0, 0, width, height))
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
//    this match {
//      case block: BlockFrag.Stack =>
//        // TODO what if there is no document?
//        val b = block.frags.find(a => a.top + a.size.height > ypos).getOrElse(block.frags.last)
//        b.pointedLine(xpos - b.left, ypos - b.top)
//      case l: LineFrag =>
//        (l, xpos)
//    }
    ???
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
  def width: Float = measure.width
  def height: Float = measure.y + measure.my

  override def doMeasure(before: TextMeasure): Unit = {}

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
    var index = tokens.indexWhere(a => a._2 <= xpos && a._2 + a._1.width > xpos)
    if (index < 0) {
      index = tokens.zipWithIndex.map(a0 => {
        val a = a0._1
        val dis = Math.abs(a._2 - xpos) min Math.abs(a._1.width + a._2 - xpos)
        (dis, a0._2)
      }).minBy(_._1)(Ordering.Float.TotalOrdering)._2
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
    } else if (x + t.width < xpos) {
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
        if (Math.abs(xpos - x - t.width) < pdiff) {
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

  sealed trait Atomic extends LineFrag {
    def frags: Seq[Frag] = Seq.empty
  }

  class Text(val text: String, val style: TextStyle, pad: Float = 0) extends Atomic {
    def editable: Boolean = node != null && node.editable

    // TODO use codepoint index, or even better grapheme index
    def measurePrefix(small: Int): Float =
      if (small == 0) 0
      else if (small == text.size) measure.width
      else style.measure(text.take(small)).width + pad

    lazy val measure = {
      val res = style.measure(text)
      if (pad == 0) {
        res
      } else {
        res.copy(width = res.width + pad * 2)
      }
    }

    def count = 1

    def parentNodeWithRelativeIndex(): (Node, Int) = {
      parentNodeWithRelativeIndexInner()
    }

    override def render(canvas: Canvas): Unit =
      canvas.draw(text, style, pad, measure.y)

  }

  class Pad(width: Float) extends Atomic {
    val measure: TextMeasure = TextMeasure(width, 0, 0)
    override def render(canvas: Canvas): Unit = {}
    def count = 0
  }

  def measureFrags(parent: Frag, frags: Seq[LineFrag], w0: Float, h0: Float) = {
    var y = 0F
    var my = 0F
    var i = 0
    while (i < frags.size) {
      val frag = frags(i)
      frag.parent = parent
      y = y max frag.measure.y
      my = my max frag.measure.my
      i += 1
    }
    var width = w0
    i = 0
    while (i < frags.size) {
      val frag = frags(i)
      frag.left = width
      width += frag.measure.width
      frag.top = y - frag.measure.y + h0
      i += 1
    }
    TextMeasure(width - w0, my, y)
  }
  class Compose(override val frags: Seq[LineFrag]) extends LineFrag with HasChildFrag {
    lazy val measure: TextMeasure = measureFrags(this, frags, 0, 0)
  }
}

sealed trait BlockFrag extends Frag {

  def lastLineWidth: Float

  def width = _width
  def height = _height
  var _width = 0F
  var _height = 0F
  var carveLeftTop: TextMeasure = null
  var lastLine: TextMeasure = null
}

object BlockFrag {

  class Compose(override val frags: Seq[Frag]) extends BlockFrag with HasChildFrag {
    assert(frags.exists(_.isInstanceOf[BlockFrag]))
    lazy val lastLineWidth: Float = {
      val lines = frags.reverse.takeWhile(_.isInstanceOf[LineFrag])
      dependentCast[BlockFrag](frags(frags.size - lines.size - 1)).lastLineWidth + lines.map(_.width).sum
    }

    override protected def doMeasure(before0: TextMeasure): Unit = {
      carveLeftTop = null
      var height = 0F
      var width = 0F
      var before = before0
      val lines = mutable.ArrayBuffer[LineFrag]()
      for (f <- frags) {
        f match {
          case frag: LineFrag =>
            lines.append(frag)
          case frag: BlockFrag =>
            val b1 = LineFrag.measureFrags(this, lines.toSeq, before.width, height)
            frag.measure(b1)
            frag.top = height
            frag.left = 0
            height += frag._height
            width = width max frag._width
            if (carveLeftTop == null) carveLeftTop = TextMeasure(before0.width, frag.carveLeftTop.my, frag.carveLeftTop.y)
            lines.clear()
            before = frag.lastLine
        }
      }
      val ll = LineFrag.measureFrags(this, lines.toSeq, before.width, height)
      this._width = width max (ll.width + before.width)
      this._height = height
      this.lastLine = TextMeasure(ll.width + before.width, ll.my, ll.y)
    }
  }

  class Tree(val pad: Float, start: LineFrag, val center: Seq[Frag], end: LineFrag) extends BlockFrag with HasChildFrag {


    override lazy val frags: Seq[Frag] = start +: center :+ end

    override def lastLineWidth: Float = end.width

    override protected def doMeasure(before: TextMeasure): Unit = {
      start.top = 0
      start.left = before.width
      var height = start.height
      var width = start.width + before.width
      var i = 0
      while (i < center.size) {
        val frag = center(i)
        frag.parent = this
        frag.measure(TextMeasure.empty)
        frag.left = pad
        width = (frag.width + pad) max width
        frag.top = height
        height += frag.height
        i += 1
      }
      end.top = height
      end.left = 0
      height = height + end.height
      width = width max end.width
      this._height = height
      this._width = width
      carveLeftTop = TextMeasure(before.width, start.measure.my, start.measure.y)
      lastLine = end.measure
    }

  }
}

