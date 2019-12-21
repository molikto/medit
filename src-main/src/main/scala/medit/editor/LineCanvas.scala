package medit.editor

import medit.draw.{Canvas, Rect, ShapeStyle, TextMeasure}
import medit.utils._

import scala.collection.mutable

// pad and top will be erased after render
class Line(var num: Int, var pad: Float, var top: Float, val items: Seq[LineFrag.Atomic]) {


  def flattenAtomics(xpos: Float): Seq[(LineFrag.Atomic, Float)] = {
    val buffer = mutable.Buffer[(LineFrag.Atomic, Float)]()
    var left = pad
    for (i <- items) {
      buffer.append((i, left))
      left += i.width
    }
    buffer.toSeq
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

  var createdTime = System.currentTimeMillis()

  def render(canvas: Canvas): Unit = {
    if (System.currentTimeMillis() - createdTime < 1000) {
      canvas.draw(Rect(0, top, 10, size.height), ShapeStyle.debug)
    }
    var left = pad
    for (item <- items) {
      item match {
        case text: LineFrag.Text =>
          canvas.draw(text.text, text.style, left + text.pad, top + size.y)
          left += text.textMeasure.width
        case pad: LineFrag.Pad =>
          left += pad.width
      }
    }
  }

  val size: TextMeasure = {
    var y = 0F
    var my = 0F
    var i = 0
    while (i < items.size) {
      val frag = items(i)
      y = y max frag.textMeasure.y
      my = my max frag.textMeasure.my
      i += 1
    }
    var width = 0F
    i = 0
    while (i < items.size) {
      val frag = items(i)
      width += frag.textMeasure.width
      i += 1
    }
    TextMeasure(width, my, y)
  }
}

case class Pos(line: Int, before: Int)

class LineCanvas() {

  def num = lines.size

  var pendingBreak = false
  def normalize(pos: Pos): Pos = {
    if (pos.before != 0 && lines(pos.line).items.size == pos.before) {
      Pos(pos.line + 1, 0)
    } else {
      pos
    }
  }
  // this is ok because we don't line break in boundary of frags
  def portLines(startPos0: Pos, endPos0: Pos, previous: LineCanvas, previousPad: Float): Boolean = {
    if (current.isEmpty) { // you can only port when current line is empty
      val startPos = previous.normalize(startPos0)
      val endPos = previous.normalize(endPos0)
      if (startPos.before == 0 && endPos.before == 0) {
        for (i <- startPos.line until endPos.line) {
          val l = previous.lines(i)
          l.pad = l.pad + pad - previousPad
          l.top = height
          l.num = lines.size + 1
          lines.append(l)
          height += l.size.height
        }
        pendingBreak = true
        true
      } else {
        false
      }
    } else {
      false
    }
  }

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

  def pos: Pos = Pos(lines.size, current.size)

  def break(): Unit = {
    if (pendingBreak) {
      assert(current.isEmpty)
      pendingBreak = false
    } else {
      if (current.nonEmpty) {
        val line = new Line(lines.size + 1, pad, height, current.toSeq)
        lines.append(line)
        height += line.size.height
        current.clear()
      }
    }
  }

  def pad(width: Float): Unit = {
    assert(current.isEmpty && !pendingBreak)
    pad += width
  }

  def rect(pos: Pos): Rect = {
    val line = lines(pos.line)
    val left = line.pad + line.items.take(pos.before).map(_.width).sum
    val item = line.items(pos.before)
    val top = line.top + line.size.y - item.textMeasure.y
    Rect(left, top, item.width, item.textMeasure.height)
  }


  def reverse(x: Float, y: Float) = {
    var index = lines.indexWhere(a => a.top + a.size.height > y)
    if (index == -1) index = lines.size - 1
    lines(index).pointedPos(x)
  }

  def render(canvas: Canvas): Unit = {
    for (line <- lines) {
      line.render(canvas)
    }
  }
}
