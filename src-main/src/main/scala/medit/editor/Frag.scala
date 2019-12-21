package medit.editor

import medit.draw.{Canvas, Position, Rect, Size, TextMeasure, TextStyle}
import medit.utils._

import scala.collection.mutable


/**
 * properties of line frags:
 * * width
 * * height
 * * baseline
 *
 * block frags:
 *
 */
sealed trait Frag {


  def frags: Seq[Frag]
  def count: Int

  def width: Float
  def height: Float
  def intrinsicWidth: Float = width
  def intrinsicHeight: Float = height

  def render(canvas: Canvas): Unit

  // set by parents
  var parent: Frag = null
  var left: Float = 0
  var top: Float = 0

  private var measured = false
  final def measure(): Unit = {
    if (!measured) {
      measured = true
      doMeasure()
    }
  }

  protected def doMeasure(): Unit

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
        if (index == 0) (frag, Rect(0, 0, frag.width, frag.height))
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
  lazy val count: Int = frags.map(_.count).sum

  def render(canvas: Canvas): Unit = {
    for (f <- frags) {
      canvas.save()
      canvas.translate(f.left, f.top)
      f.render(canvas)
      canvas.restore()
    }
  }
}

sealed trait LineFrag extends Frag {
  def textMeasure: TextMeasure
  def width: Float = textMeasure.width
  def height: Float = textMeasure.height

  override def doMeasure(): Unit = {}

  // TODO take into account of overlapping x-range, for example a/b

  // returns all atomics and x position
  def flattenAtomics(xpos: Float): Seq[(LineFrag.Atomic, Float)] = this match {
    case text: LineFrag.Text => Seq((text, xpos))
    case p: LineFrag.Pad => Seq((p, xpos))
    case compose: LineFrag.Compose => compose.frags.flatMap(a => a.flattenAtomics(xpos + a.left))
  }

  // returns all atomics and x position, removing pads but taking account for their x positions
  def flattenTexts(xpos: Float): Seq[(LineFrag.Text, Float, Boolean)] = {
    val res = flattenAtomics(xpos)
    val a = mutable.ArrayBuffer[(LineFrag.Text, Float, Boolean)]()
    for (r <- res) {
      r._1 match {
        case text: LineFrag.Text =>
          a.append((text, r._2, false))
        case _: LineFrag.Pad =>
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
      else if (small == text.size) textMeasure.width
      else style.measure(text.take(small)).width + pad

    lazy val textMeasure = {
      val res = style.measure(text)
      if (pad == 0) {
        res
      } else {
        res.copy(width = res.width + pad * 2)
      }
    }

    def count = 1

    def parentNodeWithRelativeIndex(): (Node, Int) = parentNodeWithRelativeIndexInner()

    override def render(canvas: Canvas): Unit = canvas.draw(text, style, pad, textMeasure.y)
  }

  class Pad(width: Float) extends Atomic {
    val textMeasure: TextMeasure = TextMeasure(width, 0, 0)
    override def render(canvas: Canvas): Unit = {}
    def count = 0
  }

  // w0, h0 is an offset to measure childs, don't affect the result TextMeasure
  def measureFrags(parent: Frag, frags: Seq[LineFrag]) = {
    var y = 0F
    var my = 0F
    var i = 0
    while (i < frags.size) {
      val frag = frags(i)
      frag.parent = parent
      y = y max frag.textMeasure.y
      my = my max frag.textMeasure.my
      i += 1
    }
    var width = 0F
    i = 0
    while (i < frags.size) {
      val frag = frags(i)
      frag.left = width
      width += frag.textMeasure.width
      frag.top = y - frag.textMeasure.y
      i += 1
    }
    TextMeasure(width, my, y)
  }
  class Compose(override val frags: Seq[LineFrag]) extends LineFrag with HasChildFrag {
    lazy val textMeasure: TextMeasure = measureFrags(this, frags)
  }
}

sealed trait BlockFrag extends HasChildFrag {

  /**
   * in general block frag looks like this
   *      ________
   * _____|      |
   * |           |
   * |     ______|
   * |____|
   *
   * we use parameters, w, h
   */

  def render(canvas: Canvas, firstLineTransX: Float, firstLineTransY: Float): Unit

  override def render(canvas: Canvas): Unit = {
    render(canvas, 0, 0)
  }

  def height : Float = _intrinsicHeight  + lastLineTransY + firstLineTransY
  def width: Float = _intrinsicWidth
  override def intrinsicHeight: Float = _intrinsicHeight
  override def intrinsicWidth: Float = _intrinsicWidth
  protected var _intrinsicWidth = 0F
  protected var _intrinsicHeight = 0F
  var firstLine: TextMeasure = null
  var lastLine: TextMeasure = null
  // this exists separately because we don't want to prematurely measure too much
  // it should be equal to lastLine.width after measure
  def lastLineWidth: Float
  // in addition to top, left set by parents, we have another set by parents, lastline gap
  // this is because composing might need lastline move it's baseline etc.
  protected var lastLineTransY: Float = 0 // they are all positive
  protected var firstLineTransX: Float = 0
  protected var firstLineTransY: Float = 0 // they are all positive
}

object BlockFrag {

  class Compose(override val frags: Seq[Frag]) extends BlockFrag with HasChildFrag {
    assert(frags.exists(_.isInstanceOf[BlockFrag]))
    lazy val lastLineWidth: Float = {
      val lines = frags.reverse.takeWhile(_.isInstanceOf[LineFrag])
      dependentCast[BlockFrag](frags(frags.size - lines.size - 1)).lastLineWidth +
          lines.map(a => dependentCast[LineFrag](a).width).sum
    }

    override protected def doMeasure(): Unit = {
      firstLine = null
      lastLine = TextMeasure.empty
      var height = 0F
      var width = 0F
      var before = TextMeasure.empty
      var blockBefore: BlockFrag = null
      val lines = mutable.ArrayBuffer[LineFrag]()
      def doBeforeMiddleAfter(b1: TextMeasure, after: TextMeasure): TextMeasure = {
        val yMax = before.y max b1.y max after.y
        val myMax = before.my max b1.my max after.my
        if (blockBefore != null) blockBefore.lastLineTransY = yMax - before.y
        lines.foreach(l => {
          l.top += height + yMax - b1.y
          l.left += before.width
        })
        TextMeasure(b1.width + after.width, myMax, yMax)
      }
      for (f <- frags) {
        f match {
          case f: LineFrag =>
            lines.append(f)
          case f: BlockFrag =>
            val b1 = LineFrag.measureFrags(this, lines.toSeq)
            f.measure()
            val after = f.firstLine
            f.left = 0
            val tt = doBeforeMiddleAfter(b1, after)
            if (firstLine == null) firstLine = tt
            val gapTop = tt.y - after.y
            val gapBottom = tt.my - after.my
            f.top = height + gapTop + gapBottom
            f.firstLineTransY = gapBottom
            f.firstLineTransX = before.width + b1.width
            width = width max f.width max (before.width + b1.width + after.width)
            height = f.top + f.height - f.lastLine.height
            lastLine = f.lastLine
            before = f.lastLine
            lines.clear()
        }
      }
      val b1 = LineFrag.measureFrags(this, lines.toSeq)
      lastLine = doBeforeMiddleAfter(b1, TextMeasure.empty)
      _intrinsicHeight = height + lastLine.height
      _intrinsicWidth = width
      lines.clear()
    }

    /**
     * in general block frag looks like this
     * ________
     * _____|      |
     * |           |
     * |     ______|
     * |____|
     *
     * we use parameters, w, h
     */
    override def render(canvas: Canvas, firstLineExtraX: Float, firstLineExtraY: Float): Unit = {
      var firstLine = true
      for (f <- frags) {
        canvas.save()
        val extraX = if (firstLine) firstLineExtraX + firstLineTransX else 0
        val extraY = if (firstLine) firstLineExtraY + firstLineTransY else 0
        f match {
          case _: LineFrag =>
            canvas.translate(f.left + extraX, f.top - extraY)
            f.render(canvas)
            canvas.restore()
          case b: BlockFrag =>
            canvas.translate(f.left, f.top)
            b.render(canvas, extraX, extraY)
            canvas.restore()
            firstLine = false
        }
      }
    }
  }

  class Tree(val pad: Float, start: LineFrag, val center: Seq[Frag], end: LineFrag) extends BlockFrag with HasChildFrag {


    override lazy val frags: Seq[Frag] = start +: center :+ end

    override def lastLineWidth: Float = end.width

    override def render(canvas: Canvas, firstLineExtraX: Float, firstLineExtraY: Float): Unit = {
      canvas.save()
      canvas.translate(
        start.left + firstLineExtraX + firstLineTransX,
        start.top - firstLineExtraY - firstLineTransY)
      start.render(canvas)
      canvas.restore()
      for (f <- center) {
        canvas.save()
        canvas.translate(f.left, f.top)
        f.render(canvas)
        canvas.restore()
      }
      canvas.save()
      canvas.translate(end.left, end.top)
      end.render(canvas)
      canvas.restore()
    }

    override protected def doMeasure(): Unit = {
      start.top = 0
      start.left = 0
      var height = start.height
      var width = start.width
      var i = 0
      while (i < center.size) {
        val frag = center(i)
        frag.parent = this
        frag.measure()
        frag.left = pad
        frag match {
          case frag: BlockFrag =>
            frag.lastLineTransY = 0
            frag.firstLineTransX = 0
            frag.firstLineTransY = 0
          case _: LineFrag =>
        }
        width = (frag.width + pad) max width
        frag.top = height
        height += frag.height
        i += 1
      }
      end.top = height
      end.left = 0
      height += end.height
      width = width max end.width
      firstLine = start.textMeasure
      lastLine = end.textMeasure
      this._intrinsicHeight = height
      this._intrinsicWidth = width
    }

  }
}

