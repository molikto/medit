package medit.editor

import medit.draw.{Canvas, Rect, ShapeStyle, TextMeasure}
import medit.utils._

import scala.collection.mutable


// pad and top will be erased after render
class Line(var num: Int, var pad: Float, var top: Float, val items: Seq[LineFrag.Atomic]) {

  def get(pos0: Int): (TextMeasure, Seq[TextPos]) = {
    var pos = pos0
    var line = 0
    var ret: (TextMeasure, Seq[TextPos]) = null
    var left = 0F
    while (line < items.size && ret == null) {
      val ll = items(line)
      val lsize = ll.size
      def lpos = new TextPos(dependentCast[LineFrag.Text](ll), pos)
      if (lsize == pos) {
        val nn = if (line + 1 < items.size) items(line + 1) else null
        def rpos = new TextPos(dependentCast[LineFrag.Text](nn), 0)
        left += ll.measure.width
        val mm = ll.measure.copy(width = left)
        (ll, nn) match {
          case (_: LineFrag.Text, _: LineFrag.Text) =>
            ret = (ll.measure.avg(nn.measure).copy(width = left), Seq(lpos, rpos))
          case (_: LineFrag.Text, _) =>
            ret = (ll.measure.copy(width = left), Seq(lpos))
          case (_, _: LineFrag.Text) =>
            ret = (nn.measure.copy(width = left), Seq(rpos))
          case _ =>
            logicError()
        }
      } else if (pos < lsize) {
        ll match {
          case tt: LineFrag.Text =>
            ret = (ll.measure.copy(width = left + tt.measurePrefix(pos)), Seq(lpos))
          case _ =>
            logicError()
        }
      } else {
        pos -= lsize
        line += 1
        left += ll.measure.width
      }
    }
    ret
  }

  private def flattenAtomics(xpos: Float): Seq[(LineFrag.Atomic, Float)] = {
    val buffer = mutable.Buffer[(LineFrag.Atomic, Float)]()
    var left = pad
    for (i <- items) {
      buffer.append((i, left))
      left += i.width
    }
    buffer.toSeq
  }

  private def flattenTexts(xpos: Float): Seq[(LineFrag.Text, Float, Boolean)] = {
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

  def pointedPos(xpos: Float): Int = {
    val (before, t, x, after) = pointedText0(xpos)
    val pos = if (xpos <= x) {
      0
    } else if (x + t.width < xpos) {
      t.size
    } else {
      var res = -1
      var i = 1
      var pdiff = xpos - x
      while (i < t.size && res == -1) {
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
          res = t.size
        } else {
          res = t.size - 1
        }
      }
      res
    }
    items.takeWhile(_ != t).map(_.size).sum + pos
  }

  var createdTime = System.currentTimeMillis()

  def render(canvas: Canvas): Unit = {
    if (System.currentTimeMillis() - createdTime < 1000) {
      canvas.draw(Rect(0, top, 10, measure.height), ShapeStyle.debug)
    }
    var left = pad
    for (item <- items) {
      item match {
        case text: LineFrag.Text =>
          canvas.draw(text.text, text.style, left + text.pad, top + measure.y)
          left += text.measure.width
        case pad: LineFrag.Pad =>
          left += pad.width
      }
    }
  }

  val size = items.map(_.size).sum

  val measure: TextMeasure = {
    var y = 0F
    var my = 0F
    var i = 0
    while (i < items.size) {
      val frag = items(i)
      y = y max frag.measure.y
      my = my max frag.measure.my
      i += 1
    }
    var width = 0F
    i = 0
    while (i < items.size) {
      val frag = items(i)
      width += frag.measure.width
      i += 1
    }
    TextMeasure(width, my, y)
  }
}

class TextPos(val text: LineFrag.Text, val pos: Int)
class IndexInfo(val rect: Rect, val pos: Seq[TextPos])

case class Index(index: Int, start: Boolean) {
  def inc = Index(index + 1, start)
}


class Page() {

  def num = lines.size

  var pendingBreak = false

  private val lines = mutable.Buffer[Line]()
  private var height = 0F
  private val current = mutable.Buffer[LineFrag.Atomic]()
  var pad = 0F
  def append(a: LineFrag.Atomic): Unit = {
    if (pendingBreak) {
      val last = lines.last
      lines.dropRightInPlace(1)
      assert(current.isEmpty)
      current.appendAll(last.items)
    }
    current.append(a)
  }

  def break(): Unit = {
    if (pendingBreak) {
      assert(current.isEmpty)
      pendingBreak = false
    } else {
      if (current.nonEmpty) {
        val line = new Line(lines.size + 1, pad, height, current.toSeq)
        lines.append(line)
        height += line.measure.height
        current.clear()
      }
    }
  }

  def pad(width: Float): Unit = {
    assert(current.isEmpty && !pendingBreak)
    pad += width
  }

  def get(index: Index): IndexInfo = {
    var top = 0F
    var pos = index.index
    val start = index.start
    var line = 0
    var ret: IndexInfo = null
    while (line < lines.size && ret == null) {
      val ll = lines(line)
      val lsize = ll.size
      if (lsize < pos || (lsize == pos && start)) {
        pos -= lsize
        line += 1
        top += ll.measure.height
      } else {
        val (mm, ii) = ll.get(pos)
        val rect = Rect(ll.pad + mm.width, top + ll.measure.y - mm.y, 3, mm.height)
        ret = new IndexInfo(rect, ii)
      }
    }
    ret
  }

  def reverse(x: Float, y: Float): Index = {
    var index = lines.indexWhere(a => a.top + a.measure.height > y)
    if (index == -1) index = lines.size - 1
    val pos2 = lines(index).pointedPos(x)
    val before = lines.take(index).map(_.size).sum
    Index(before + pos2, pos2 == 0)
  }

  def render(canvas: Canvas): Unit = {
    for (line <- lines) {
      line.render(canvas)
    }
  }
}
