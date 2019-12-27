package medit.editor

import medit.draw.{TextMeasure, TextStyle}
import medit.structure.SeparatorOpts
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
  def size: Int

  // set by parents
  var parent: Frag = null

  var node: Node = null

  def parentNode(): Node = if (node == null) parent.parentNode() else node

  protected def bone0(): Seq[Frag] = if (this.node != null) Seq(this) else bone()

  def bone(): Seq[Frag] = this match {
    case compose: LineFrag.Compose =>
      compose.frags.flatMap(_.bone0())
    case compose: BlockFrag.Compose =>
      compose.frags.flatMap(_.bone0())
    case tree: BlockFrag.Tree =>
      tree.start.bone0() ++ tree.center.flatMap(_.bone0()) ++ tree.end.bone0()
    case text: LineFrag.Text =>
      Seq(text)
    case _: LineFrag.Pad =>
      Seq.empty
  }
}

sealed trait NonAtomicFrag extends Frag {
  lazy val count: Int = frags.map(_.count).sum

  lazy val size: Int = frags.map(_.size).sum

  def setParents() = frags.foreach(_.parent = this)

  def finishInner(canvas: Page): Unit

  final override def finish(canvas: Page): Unit = {
    // FIXME we remember pos before but it is not that sound, instead we should do a second pass of lines
    finishInner(canvas)
  }

}

sealed trait LineFrag extends Frag {
  def width: Float
}
object LineFrag {

  sealed trait Atomic extends LineFrag {
    def frags: Seq[Frag] = Seq.empty

    def measure: TextMeasure

    override def finish(canvas: Page): Unit = canvas.append(this)
  }

  class Text(
      val text: String,
      val style: TextStyle,
      val emptyAsQuestionMark: Boolean = false,
      val hideInLineEnd: Boolean = false,
      val pad: Float = 0) extends Atomic {

    def resolve(): FragPart = {
      parentNode() match {
        case node: Node.Structure =>
          val bone = node.frag.bone()
          PartOfStructure.ConstantAt(node, bone.indexOf(this), bone.size)
        case node: Node.Collection =>
          val bone = node.frag.bone()
          if (this == bone.head) {
            PartOfCollection.B1(node)
          } else if (this == bone.last) {
            PartOfCollection.B2(node)
          } else {
            val bs = bone.dropWhile(_.node == null)
            val index = bs.indexOf(this)
            if (index % 2 != 1) {
              logicError()
            }
            PartOfCollection.SeparatorAt(node, index / 2)
          }
        case leaf: Node.EditableLeaf =>
          assert(leaf == node)
          JustStringNode(leaf)
      }
    }

    def size: Int = text.size

    // TODO use codepoint index, or even better grapheme index
    def measurePrefix(small: Int): Float =
      if (small == 0) 0
      else if (small == text.size) measure.width
      else style.measure(text.take(small)).width + pad

    lazy val measure = {
      if (text.isEmpty && emptyAsQuestionMark) {
        val res = style.measure("?")
        if (pad == 0) {
          res
        } else {
          res.copy(width = res.width + pad * 2)
        }
      } else {
        val res = style.measure(text)
        if (pad == 0) {
          res
        } else {
          res.copy(width = res.width + pad * 2)
        }
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

  class Tree(val pad: Float, val start: LineFrag, val center: Seq[Frag], val end: LineFrag) extends BlockFrag with NonAtomicFrag {
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

