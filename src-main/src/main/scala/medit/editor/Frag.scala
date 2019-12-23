package medit.editor

import medit.draw.{TextMeasure, TextStyle}
import medit.utils._



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

  def finish(canvas: Page): Unit

  def frags: Seq[Frag]
  def count: Int

  // set by parents
  var parent: Frag = null
}

sealed trait NonAtomicFrag extends Frag {
  lazy val count: Int = frags.map(_.count).sum

  def setParents() = frags.foreach(_.parent = this)

  def finishInner(canvas: Page): Unit

  final override def finish(canvas: Page): Unit = {
    // TODO we remember pos before but it is not that sound
    finishInner(canvas)
  }
}

sealed trait LineFrag extends Frag {
  def width: Float
}
object LineFrag {

  sealed trait Atomic extends LineFrag {
    def frags: Seq[Frag] = Seq.empty

    def size: Int
    def measure: TextMeasure

    override def finish(canvas: Page): Unit = canvas.append(this)
  }

  class Text(
      val text: String,
      val style: TextStyle,
      @nullable val node: Node.EditableLeaf = null,
      val pad: Float = 0) extends Atomic {
    def editable: Boolean = node != null

    def size: Int = text.size

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

    override def width: Float = measure.width

    def count = 1
  }

  class Pad(override val width: Float) extends Atomic {
    val measure: TextMeasure = TextMeasure(width, 0, 0)
    def count = 0
    def size = 1
  }

  class Compose(override val frags: Seq[LineFrag]) extends LineFrag with NonAtomicFrag {
    setParents()
    override def width: Float = frags.map(_.width).sum

    override def finishInner(canvas: Page): Unit = frags.foreach(_.finish(canvas))
  }
}

sealed trait BlockFrag extends NonAtomicFrag {

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

  class Compose(override val frags: Seq[Frag]) extends BlockFrag with NonAtomicFrag {
    assert(frags.exists(_.isInstanceOf[BlockFrag]))
    setParents()
    lazy val lastLineWidth: Float = {
      val lines = frags.reverse.takeWhile(_.isInstanceOf[LineFrag])
      dependentCast[BlockFrag](frags(frags.size - lines.size - 1)).lastLineWidth +
          lines.map(a => dependentCast[LineFrag](a).width).sum
    }

    override def finishInner(canvas: Page): Unit = frags.foreach(_.finish(canvas))
  }

  class Tree(val pad: Float, start: LineFrag, val center: Seq[Frag], end: LineFrag) extends BlockFrag with NonAtomicFrag {
    override lazy val frags: Seq[Frag] = start +: center :+ end
    setParents()

    override def lastLineWidth: Float = end.width

    override def finishInner(canvas: Page): Unit = {
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

