package medit.editor

import medit.draw
import medit.draw.{DrawCall, Position, Rect, Size, TextMeasure, TextStyle}
import medit.structure.{Data, Language, Linear, Template, Type, TypeTag}

import scala.collection.mutable
import medit.utils._

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {

  protected var _parent: Node
  @nullable def parent: Node = _parent
  def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else logicError()
  def childs: Seq[Node]

  def tryNewChild(): Int = -1
  def tryEdit(): Boolean = false
  def editAppend(c: Int): Unit = logicError()
  def editBackspace(): Unit = logicError()
  def editCommit(): Unit = logicError()

  // all in parent coordinate
  protected var top: Float = 0
  protected var left: Float = 0
  protected var _draw: DrawCall = null
  protected var baseline: Float = -1
  def multiline = baseline < 0
  protected var size: Size = null

  def rect = Rect(top, left, size.height, size.width)

  // layout will determine itself the size and multiline, then top left is assgined by parent
  def layout(width: Int): Unit
  def draw: DrawCall = _draw
}

object Node {

  val DefaultIndent = 8

  def create(parent: Node, language: Language, a: TypeTag, data: ujson.Value): Node = a match {
    case TypeTag.Str => new Str(parent).init(data.str)
    case coll: TypeTag.Coll =>
      val col = new Collection(parent, language, coll)
      col.init(data.arr.toSeq.map(a => create(col, language, coll.item, a)))
      col
    case n: TypeTag.Ref => language.types(n.index) match {
      case record: Type.Record =>
        val s = new Structure(parent, language, n, -1)
        val obj = data.obj
        s.init(record.fields.map(f => create(s, language, f.tag, obj(f.name))))
        s
      case sum: Type.Sum =>
        val obj = data.obj
        val typ = obj("$type").str
        val id = sum.cases.indexWhere(_.name == typ)
        val s = new Structure(parent, language, n, id)
        s.init(sum.cases(id).fields.map(f => create(s, language, f.tag, obj(f.name))))
        s
    }
  }
  def default(parent: Node, language: Language, a: TypeTag): Node = a match {
    case TypeTag.Str => new Str(parent)
    case coll: TypeTag.Coll => new Collection(parent, language, coll)
    case n: TypeTag.Ref => language.types(n.index) match {
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

  type LayoutRes = (draw.DrawCall, Size, Float)

  def layoutLinear(left: Seq[Linear]): LayoutRes = {
    var calls = Seq.empty[DrawCall.Text]
    var y = 0F
    var my = 0F
    left.foreach {
      case Linear.Keyword(name) =>
        val style = TextStyle.keyword
        val measure = style.measure(name)
        y = measure.y max y
        my = measure.my max my
      case Linear.Delimiter(str) =>
        val style = TextStyle.delimiters
        val measure = style.measure(str)
        y = measure.y max y
        my = measure.my max my
    }
    var width = 0F
    left.foreach {
      case Linear.Keyword(name) =>
        val style = TextStyle.keyword
        val measure = style.measure(name)
        calls = calls :+ DrawCall.Text(Position(y, width, 0), style, name)
        width += measure.w
      case Linear.Delimiter(str) =>
        val style = TextStyle.delimiters
        val measure = style.measure(str)
        calls = calls :+ DrawCall.Text(Position(y, width, 0), style, str)
        width += measure.w
    }
    (DrawCall.Group(calls), Size(y + my, width), y)
  }



  @inline def layoutComposite(left: Seq[Linear], cs0: Seq[LayoutRes], sep: Seq[Linear], end: Seq[Linear], width: Int): LayoutRes = {
    val (pc, p, pb) = layoutLinear(left)
    val cs = cs0.map(_._2.width).sum
    val (sc, s, sb) = layoutLinear(sep)
    val (ec, e, eb) = layoutLinear(end)
    if (!cs0.exists(_._3 < 0) && p.width + cs + s.width * cs0.size + e.width <= width) {
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
      (DrawCall.Group(calls), Size(ymax + mymax, left), ymax)
    } else {
      var calls = Seq(pc)
      var width = p.width
      var top = p.height
      cs0.foreach(c => {
        calls = calls :+ c._1.translated(Position(top, Node.DefaultIndent, 0))
        top += c._2.height
        width = (Node.DefaultIndent + c._2.width) max width
        // TODO add seperator
      })
      calls = calls :+ ec.translated(Position(top, 0, 0))
      top += e.height
      width = width max e.width
      (DrawCall.Group(calls), Size(top, width), -1)
    }
  }

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



  }

  class Structure(
                   override protected var _parent: Node,
                   val language: Language, val tag: TypeTag.Ref, val index: Int) extends HaveChilds {
    def typ = language.types(tag.index)
    def childTypes = typ(index)

    val template: Template = typ match {
      case record: Type.Record =>
        record.templateOrDefault
      case sum: Type.Sum =>
        sum.cases(index).templateOrDefault
    }

    def layout(template: Template, width: Int): LayoutRes = {
      template match {
        case Template.Field(i) =>
          val c = childs(i)
          c.layout(width)
          (c.draw, c.size, c.baseline)
        case Template.Tree(left, b1, content, sep, b2) =>
          layoutComposite(left ++ b1,
            content.map(a => layout(a, width - Node.DefaultIndent)),
            sep.toSeq, b2.toSeq, width)
        case Template.Literal(contents) =>
          layoutLinear(contents)
        case _ => logicError()
      }
    }

    override def layout(width: Int): Unit = {
      val res = layout(template, width)
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


    override def layout(width: Int): Unit = {
      val left = Seq(Linear.Delimiter("["))
      val sep = Seq(Linear.Delimiter(","))
      val end = Seq(Linear.Delimiter("]"))
      childs.foreach(a => a.layout(width - Node.DefaultIndent))
      val cs0 = childs.map(a => (a._draw, a.size, a.baseline))
      val res = layoutComposite(left, cs0, sep, end, width)
      _draw = res._1
      size = res._2
      baseline = res._3
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


    override def layout(width: Int): Unit = {
      val ts = TextStyle.reference.measure(buffer)
      baseline = ts.y
      size = Size(ts.y + ts.my, ts.w)
      _draw = DrawCall.Text(Position(baseline, 0, 0), TextStyle.reference, buffer)
    }
  }

  class Choice(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Ref
  ) extends StringLeaf {
    def typ = language.types(tag.index).asInstanceOf[Type.Sum]

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
  }

  class Str(
      override protected var _parent: Node
  ) extends StringLeaf {
    def init(str: String): Node = {
      buffer = str
      this
    }

    override def tryCommit(buffer: String): Boolean = false
  }
}
