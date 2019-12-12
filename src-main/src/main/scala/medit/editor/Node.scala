package medit.editor

import medit.{draw, editor}
import medit.draw.{DrawCall, Position, Rect, Size, TextMeasure, TextStyle}
import medit.structure.Template.{LeftPad, RightPad}
import medit.structure.{Data, Language, NameTypeTag, Template, Type, TypeTag}

import scala.collection.mutable
import medit.utils._
import ujson.Value

import scala.collection.immutable.{AbstractSeq, LinearSeq}

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {

  def save(): ujson.Value

  protected var _parent: Node
  @nullable def parent: Node = _parent
  def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else logicError()
  def childs: Seq[Node]

  def tryDelete(last: Int) = false
  def tryNewChild(): Int = -1
  def tryEdit(): Boolean = false
  def editAppend(c: Int): Unit = logicError()
  def editBackspace(): Unit = logicError()
  def editCommit(): Unit = logicError()

  // all in parent coordinate
  protected var top: Float = 0
  protected var left: Float = 0
  protected var _draw: DrawTemplate = null
  protected var baseline: Float = -1
  def multiline = baseline < 0
  protected var size: Size = null

  def rect = Rect(top, left, size.height, size.width)
  def rect(focus: Seq[Int]): Rect = {
    if (focus.isEmpty) {
      rect
    } else {
      childs(focus.head).rect(focus.tail) + Position(top, left, 0)
    }
  }

  // layout will determine itself the size and multiline, then top left is assgined by parent
  def layout(width: Float, forceLinear: Boolean): Unit

  def flatten(_draw: DrawTemplate): DrawCall = _draw match {
    case DrawTemplate.Child(i) => childs(i).draw
    case DrawTemplate.Translated(position, temps) =>
      DrawCall.Translated(position, temps.map(flatten))
    case DrawTemplate.Group(temps) =>
      DrawCall.Group(temps.map(flatten))
    case DrawTemplate.Just(call) =>
      call
  }

  def draw: DrawCall = flatten(_draw)
}

object Node {

  val DefaultIndent: Float = TextStyle.delimiters.unit.w * 2

  def create(parent: Node, language: Language, a: TypeTag, data: ujson.Value): Node = {
    def fromFields(s: Structure, fields: Seq[NameTypeTag], obj: mutable.Map[String, ujson.Value]): Unit = {
      s.init(fields.map(f => {
        if (obj.contains(f.name)) {
          create(s, language, f.tag, obj(f.name))
        } else {
          default(s, language, f.tag)
        }
      }))
    }
    a match {
      case TypeTag.Str => new Str(parent).init(data.str)
      case coll: TypeTag.Coll =>
        val col = new Collection(parent, language, coll)
        col.init(data.arr.toSeq.map(a => create(col, language, coll.item, a)))
        col
      case n: TypeTag.Ref => language.types(n.index) match {
        case record: Type.Record =>
          val s = new Structure(parent, language, n, -1)
          fromFields(s, record.fields, data.obj)
          s
        case sum: Type.Sum =>
          if (data.obj.contains("$choice")) {
            new Choice(parent, language, n).init(data.obj("$choice").str)
          } else {
            val obj = data.obj
            val typ = obj("$type").str
            val id = sum.cases.indexWhere(_.name == typ)
            val s = new Structure(parent, language, n, id)
            fromFields(s,sum.cases(id).fields, obj)
            s
          }
      }
    }
  }

  def default(parent: Node, language: Language, a: TypeTag): Node = a match {
    case TypeTag.Str => new Str(parent)
    case coll: TypeTag.Coll => new Collection(parent, language, coll)
    case n: TypeTag.Ref =>
      if (n.index == -1) {
        logicError()
      }
      language.types(n.index) match {
        case record: Type.Record =>
          val s = new Structure(parent, language, n, -1)
          s.init(record.fields.map(f => default(s, language, f.tag)))
          s
        case sum: Type.Sum =>
          new Choice(parent, language, n)
      }
  }

  def root(a: Language): Node = {
    default(null, a, a.root)
  }

  type LayoutRes = (DrawTemplate, Size, Float)




  sealed trait HaveChilds extends Node {
    protected val _childs = new mutable.ArrayBuffer[Node]()

    def init(childs: Seq[Node]): Node= {
      _childs.appendAll(childs)
      this
    }
    override def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else _childs(focus.head)(focus.tail)
    override def childs: Seq[Node] = _childs.toSeq

    private[Node] def replace(choice: Node, s: Node) = {
      assert(choice.parent == this && s.parent == null)
      val index = childs.indexOf(choice)
      assert(index >= 0)
      _childs.update(index, s)
      s._parent = this
    }


    def resolveChildPositions(_1: DrawTemplate, position: Position = Position.unit): Unit = _1 match {
      case DrawTemplate.Child(i) =>
        if (childs.size <= i) {
          logicError()
        }
        val c = childs(i)
        c.left = position.left
        c.top = position.top
      case DrawTemplate.Group(temps) =>
        temps.foreach(a => resolveChildPositions(a, position))
      case DrawTemplate.Translated(pos, temps) =>
        temps.foreach(a => resolveChildPositions(a, position + pos))
      case _: DrawTemplate.Just =>
    }

    def layoutLinear(left: Seq[Template]) : LayoutRes = {
      if (left.isEmpty) {
        (DrawTemplate.Group(Seq.empty), Size(0, 0), 0)
      } else {
        var calls = Seq.empty[DrawTemplate]
        val res = left.map({ l =>
          layout(l, 0, true)
        })
        val y = res.map(_._3).max
        val my = res.map(a => a._2.height - a._3).max
        var width = 0F
        res.foreach({ l =>
          calls = calls :+ l._1.translated(Position(y  - l._3, width, 0))
          width += l._2.width
        })
        (DrawTemplate.Group(calls), Size(y + my, width), y)
      }
    }

    @inline def layoutComposite(left: Seq[Template], cs0: Seq[LayoutRes], sep: Seq[Template], end: Seq[Template], width: Float, forceLinear: Boolean): LayoutRes = {
      val (pc, p, pb) = layoutLinear(left)
      val cs = cs0.map(_._2.width).sum
      val (sc, s, sb) = layoutLinear(sep)
      val (ec, e, eb) = layoutLinear(end)
      if (forceLinear || (!cs0.exists(_._3 < 0) && p.width + cs + s.width * cs0.size + e.width <= width)) {
        var ymax = pb max eb max sb
        var mymax = (p.height - pb) max (e.height - eb) max (s.height - sb)
        cs0.foreach(a => {
          ymax = a._3 max ymax
          mymax = (a._2.height - a._3) max mymax
        })
        var left = p.width
        var calls = Seq(pc.translated(Position(ymax - pb, 0, 0)))
        var i = 0
        cs0.foreach(c => {
          calls = calls :+ c._1.translated(Position(ymax - c._3, left, 0))
          left = left + c._2.width
          if (i != cs0.size - 1) {
            calls = calls :+ sc.translated(Position(ymax - sb, left, 0))
            left += s.width
          }
          i += 1
        })
        calls = calls :+ ec.translated(Position(ymax - eb, left, 0))
        left += e.width
        (DrawTemplate.Group(calls), Size(ymax + mymax, left), ymax)
      } else {
        val (pc, p, _) = layoutLinear(left.reverse.dropWhile(_ == RightPad).reverse)
        val (ec, e, _) = layoutLinear(end.dropWhile(_ == LeftPad))
        var calls = Seq(pc)
        var width = p.width
        var top = p.height
        cs0.foreach(c => {
          calls = calls :+ c._1.translated(Position(top, Node.DefaultIndent, 0))
          top += c._2.height
          width = (Node.DefaultIndent + c._2.width) max width
          // FIXME add seperator
        })
        calls = calls :+ ec.translated(Position(top, 0, 0))
        top += e.height
        width = width max e.width
        (DrawTemplate.Group(calls), Size(top, width), -1)
      }
    }

    def layoutText(style: TextStyle, name: String, leftPad: Float, rightPad: Float) = {
      val measure = style.measure(name)
      (DrawTemplate.Just(DrawCall.Text(Position(measure.y, leftPad, 0), style, name)), Size(measure.y + measure.my, measure.w + leftPad + rightPad), measure.y)
    }

    def layout(left: Template, width: Float, forceLinear: Boolean): LayoutRes = {
      left match {
        case Template.Keyword(name) =>
          layoutText(TextStyle.keyword, name, 0F, 0F)
        case Template.Separator(name) =>
          val style = TextStyle.delimiters
          val unit = style.unit.w / 3
          layoutText(style, name, unit, unit)
        case Template.RightPad  =>
          layoutText(TextStyle.delimiters, " ", 0F, 0F)
        case Template.LeftPad  =>
          layoutText(TextStyle.delimiters, " ", 0F, 0F)
        case Template.Delimiter(str) =>
          layoutText(TextStyle.delimiters, str, 0F, 0F)
        case f: Template.Field =>
          val c = childs(f.index)
          c.layout(width, forceLinear)
          (DrawTemplate.Child(f.index), c.size, c.baseline)
        case Template.Unfold(c) =>
          layout(c, width, forceLinear) // FIXME unfold handling is bad
        case Template.Tree(left, b1, content, sep, b2) =>
          val cons = content match {
            case Seq(Template.Unfold(f@Template.Field(_))) =>
              val c = childs(f.index)
              c.asInstanceOf[Collection].layout(cs => layoutComposite(Seq.empty, cs, sep.toSeq, Seq.empty, width, forceLinear), width, forceLinear)
              Seq((DrawTemplate.Child(f.index), c.size, c.baseline))
            case _ =>
              content.map(a => layout(a, width - Node.DefaultIndent, forceLinear))
          }
          layoutComposite(left ++ b1,
            cons,
            sep.toSeq, b2, width, forceLinear)
      }
    }
  }

  class Structure(
                   override protected var _parent: Node,
                   val language: Language, val tag: TypeTag.Ref, val index: Int) extends HaveChilds {
    def typ = language.types(tag.index)
    def childTypes = typ(index)
    override def save(): Value = {
      val cs = childs.zip(childTypes).map(p => (p._2.name, p._1.save()))
      val cs0 = if (index == -1) cs else Seq(("$type", ujson.Str(typ.asInstanceOf[Type.Sum].cases(index).name))) ++ cs
      ujson.Obj.from(cs0)
    }

    val template: Template = typ match {
      case record: Type.Record =>
        record.templateOrDefault
      case sum: Type.Sum =>
        sum.cases(index).templateOrDefault
    }


    override def layout(width: Float, forceLinear: Boolean): Unit = {
      val res = layout(template, width, forceLinear)
      resolveChildPositions(res._1)
      _draw = res._1
      size = res._2
      baseline = res._3
    }

  }

  class Collection(
      override protected var _parent: Node,
      val language: Language, val sort: TypeTag.Coll) extends HaveChilds {

    override def tryNewChild(): Int = {
      // TODO check collection type and size limit
      _childs.append(default(this, language, sort.item))
      _childs.size - 1
    }


    override def tryDelete(last: Int): Boolean = {
      if (last < childs.size) {
        _childs.remove(last)
        true
      } else {
        false
      }
    }

    override def save(): Value = {
      ujson.Arr.from(childs.map(_.save()))
    }

    def layout(mapper: Seq[LayoutRes] => LayoutRes, width: Float, forceLinear: Boolean): Unit = {
      val cs0 = childs.zipWithIndex.map(a => {
        val res = a._1.layout(width, forceLinear)
        (DrawTemplate.Child(a._2), a._1.size, a._1.baseline)
      })
      val res = mapper(cs0)
      resolveChildPositions(res._1)
      _draw = res._1
      size = res._2
      baseline = res._3
    }

    override def layout(width: Float, forceLinear: Boolean): Unit = {
      val left = Seq(Template.Delimiter("["))
      val sep = Seq(Template.Separator(","))
      val end = Seq(Template.Delimiter("]"))
      layout(cs =>
        layoutComposite(left, cs, sep, end, width, forceLinear), width - Node.DefaultIndent, forceLinear)
    }
  }

  sealed trait StringLeaf extends Node {
    override def childs: Seq[Node] = Seq.empty

    protected var buffer = ""
    override def tryEdit(): Boolean = {
      buffer = ""
      true
    }
    override def editAppend(c: Int): Unit = {
      buffer = buffer + c.toChar.toString
    }

    override def editBackspace(): Unit = {
      if (buffer.nonEmpty) buffer = buffer.dropRight(1)
    }

    def tryCommit(buffer: String): Boolean

    override def editCommit(): Unit = {
      if (tryCommit(buffer)) {
        buffer = ""
      }
    }


    def style: TextStyle

    override def layout(width: Float, forceLinear: Boolean): Unit = {
      val str = if (buffer.isEmpty) "?" else buffer
      val ts = style.measure(str)
      baseline = ts.y
      size = Size(ts.y + ts.my, ts.w)
      _draw = DrawTemplate.Just(DrawCall.Text(Position(baseline, 0, 0), style, str))
    }
  }

  class Choice(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Ref
  ) extends StringLeaf {
    def init(c: String): _root_.medit.editor.Node = {
      buffer = c
      this
    }

    def typ = language.types(tag.index).asInstanceOf[Type.Sum]

    override def save(): Value = ujson.Obj.from(Seq(("$chioce", buffer)))

    override def tryCommit(buffer: String): Boolean = {
      typ.cases.indexWhere(_.name == buffer) match {
        case -1 => false
        case i =>
          val s = new Structure(null, language, tag, i)
          s.init(typ.cases(i).fields.map(a => default(s, language, a.tag)))
          parent match {
            case h: HaveChilds =>
              h.replace(this, s)
            case _ => logicError()
          }
          true
      }
    }

    override def style: TextStyle = TextStyle.reference
  }

  class Str(
      override protected var _parent: Node
  ) extends StringLeaf {

  override def style: TextStyle = TextStyle.reference

  override def save(): Value = ujson.Str(buffer)

  def init(str: String): Node = {
      buffer = str
      this
    }

    override def tryCommit(buffer: String): Boolean = false
  }
}
