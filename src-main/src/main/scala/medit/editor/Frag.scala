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
  def finish(canvas: LineCanvas): Unit

  def frags: Seq[Frag]
  def count: Int

  // set by parents
  var parent: Frag = null

  @nullable var node: Node = null // if a node is represented by this frag

  def get(index: Int): LineFrag.Text = {
    this match {
      case frag: LineFrag.Text =>
        if (index == 0) frag
        else logicError()
      case _ =>
        var i = 0
        var c = 0
        var res: LineFrag.Text = null
        while (i < frags.size && res == null) {
          val f = frags(i)
          if (c + f.count > index) {
            val rr = f.get(index - c)
            res = rr
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
  var previous: LineCanvas = null
  var previousPad: Float = 0
  var startPos: Pos = null
  var endPos: Pos = null

  def setParents() = frags.foreach(_.parent = this)

  def finishInner(canvas: LineCanvas): Unit

  final override def finish(canvas: LineCanvas): Unit = {
    val padStart = canvas.pad
    val currentStartPos = canvas.pos
    if (previous != null && canvas.portLines(startPos, endPos, previous, previousPad)) {
      startPos = currentStartPos
      endPos = canvas.pos
    } else {
      startPos = currentStartPos
      finishInner(canvas)
      endPos = canvas.pos
    }
    previous = canvas
    previousPad = padStart
  }
}

sealed trait LineFrag extends Frag {
  def width: Float
}
object LineFrag {

  sealed trait Atomic extends LineFrag {
    def frags: Seq[Frag] = Seq.empty
    var pos: Pos = null

    def textMeasure: TextMeasure

    override def finish(canvas: LineCanvas): Unit = {
      pos = canvas.pos
      canvas.append(this)
    }

    def parentNodeWithRelativeIndex(): (Node, Int) = parentNodeWithRelativeIndexInner()
  }

  class Text(val text: String, val style: TextStyle, val pad: Float = 0) extends Atomic {
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

    override def width: Float = textMeasure.width

    def count = 1


  }

  class Pad(override val width: Float) extends Atomic {
    val textMeasure: TextMeasure = TextMeasure(width, 0, 0)
    def count = 0
  }

  class Compose(override val frags: Seq[LineFrag]) extends LineFrag with HasChildFrag {
    setParents()
    override def width: Float = frags.map(_.width).sum

    override def finishInner(canvas: LineCanvas): Unit = frags.foreach(_.finish(canvas))
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

  def lastLineWidth: Float
}

object BlockFrag {

  class Compose(override val frags: Seq[Frag]) extends BlockFrag with HasChildFrag {
    assert(frags.exists(_.isInstanceOf[BlockFrag]))
    setParents()
    lazy val lastLineWidth: Float = {
      val lines = frags.reverse.takeWhile(_.isInstanceOf[LineFrag])
      dependentCast[BlockFrag](frags(frags.size - lines.size - 1)).lastLineWidth +
          lines.map(a => dependentCast[LineFrag](a).width).sum
    }

    override def finishInner(canvas: LineCanvas): Unit = frags.foreach(_.finish(canvas))
  }

  class Tree(val pad: Float, start: LineFrag, val center: Seq[Frag], end: LineFrag) extends BlockFrag with HasChildFrag {
    override lazy val frags: Seq[Frag] = start +: center :+ end
    setParents()

    override def lastLineWidth: Float = end.width

    override def finishInner(canvas: LineCanvas): Unit = {
      start.finish(canvas)
      canvas.break()
      canvas.pad(pad)
      center.foreach(a => {
        a.finish(canvas)
        canvas.break()
      })
      canvas.pad(-pad)
      end.finish(canvas)
    }
  }
}

