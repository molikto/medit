package medit.editor

import medit.{draw, editor}
import medit.draw.{Position, Rect, Size, TextMeasure, TextStyle}
import medit.structure.Template.{LeftPad, RightPad}
import medit.structure.{Language, NameTypeTag, Template, Type, TypeTag}

import scala.collection.mutable
import medit.utils._
import ujson.Value

import scala.collection.immutable.{AbstractSeq, LinearSeq}

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {


  def save(): ujson.Value

  @nullable protected var _parent: Node
  @nullable def parent: Node = _parent
  def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else logicError()
  def childs: Seq[Node]

  def editable = this.isInstanceOf[Node.StringLeaf]

  def tryDelete(last: Int) = false
  def tryNewChild(): Int = -1
  def editAppend(c: Int, small: Int): Unit = logicError()
  def editBackspace(small: Int): Unit = logicError()
  def editCommit(): Unit = logicError()

  def reverse(f: Node): Seq[Int] = {
    def rec(f: Node, acc: Seq[Int]): Seq[Int] = {
      if (f == this) {
        acc
      } else {
        val p = f.parent
        val i = p.childs.indexOf(f)
        rec(p, i +: acc)
      }
    }
    rec(f, Seq.empty)
  }

  @nullable def nodeEnclosing(xpos: Float, ypos: Float): Seq[Int] = {
    val f = frag.fragEnclosing(xpos, ypos)
    if (f != null) {
      val node = f.parentNode()
      reverse(node)
    } else {
      null
    }
  }

  @nullable def pointedPos(xpos: Float, ypos: Float): Mode.Insert = {
    val (f, ii) = frag.pointedPos(xpos, ypos)
    val (n, i) = f.parentNodeWithRelativeIndex()
    Mode.Insert(reverse(n), i, n.editable, ii, f.text.size)
  }

  @nullable def pointedText(xpos: Float, ypos: Float): Mode.Frag = {
    val f = frag.pointedText(xpos, ypos)
    val (n, i) = f.parentNodeWithRelativeIndex()
    Mode.Frag(reverse(n), i)
  }


  def get(focus: Seq[Int], index: Int): (LineFrag.Text, Rect) = {
    val r = rect(focus)
    val (t, rr) = apply(focus).frag.get(index)
    (t, rr + r.leftTop)
  }

  def rect(focus: Seq[Int]): Rect = {
    var left = 0F
    var top = 0F
    var fcs = focus
    var hack = frag.size
    var f = apply(focus).frag
    val size = f.size
    while (f != frag) {
      left += f.left
      top += f.top
      f = f.parent
    }
    Rect(left, top, size.width, size.height)
  }

  var frag: Frag = null
  var fragWidth: Float = -1
  var fragForceLinear: Boolean = false

  // layout will determine itself the size and multiline, then top left is assgined by parent
  def layout(width: Float, forceLinear: Boolean): Frag = {
    if (frag != null && (fragWidth != width || fragForceLinear != forceLinear)) {
      frag.node = null
      frag = null
    }
    if (frag == null) {
      frag = doLayout(width, forceLinear)
      if (frag.node == null) frag.node = this
    }
    frag
  }

  def render(canvas: draw.Canvas) = frag.render(canvas)

  protected def doLayout(width: Float, forceLinear: Boolean): Frag
}

object Node {

  val DefaultIndent: Float = TextStyle.delimiters.unit.width * 2

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
      case str@TypeTag.Str(style) => new Str(parent, str.style).init(data.str)
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
    case s@TypeTag.Str(_) => new Str(parent, s.style)
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

    def layoutLinear(left: Seq[Template]) : LineFrag = {
      if (left.isEmpty) {
        new LineFrag.Compose(Seq.empty)
      } else {
        new LineFrag.Compose(left.map(l => dependentCast[LineFrag](layout(l, 0, true))))
      }
    }

    @inline def layoutComposite(left: Seq[Template], cs: Seq[Frag], sep: Seq[Template], end: Seq[Template], width: Float, forceLinear: Boolean): Frag = {
      val leftFrag = layoutLinear(left)
      val sepFrag = layoutLinear(sep)
      val endFrag = layoutLinear(end)
      if (forceLinear || !cs.exists(_.isInstanceOf[Block])
          && leftFrag.width + cs.map(a => dependentCast[LineFrag](a).width).sum + sepFrag.width * cs.size + endFrag.width <= width) {
        new LineFrag.Compose(leftFrag +: cs.flatMap(a => Seq(layoutLinear(sep), dependentCast[LineFrag](a))).drop(1) :+ endFrag)
      } else {
        val leftFrag1 = {
          val removedPadding = left.reverse.dropWhile(_ == RightPad).reverse
          if (removedPadding.size != left.size) {
            layoutLinear(removedPadding)
          } else {
            leftFrag
          }
        }
        // FIXME add seperator
        val endFrag1 = {
          val removedPadding = end.dropWhile(_ == LeftPad)
          if (removedPadding.size != end.size) {
            layoutLinear(removedPadding)
          } else {
            endFrag
          }
        }
        new Block(0, Seq(
          leftFrag1,
          new Block(Node.DefaultIndent, cs),
          endFrag1
        ))
      }
    }

    def layout(left: Template, width: Float, forceLinear: Boolean): Frag = {
      left match {
        case Template.Keyword(name) =>
          new LineFrag.Text(name, TextStyle.keyword)
        case Template.Separator(name) =>
          val style = TextStyle.delimiters
          val unit = style.unit.width / 4F
          // we cannot use LineFrag.Pad here, as they create extra insertion point
          new LineFrag.Text(name, TextStyle.delimiters, unit)
        case Template.Pad | Template.LeftPad | Template.RightPad  =>
          new LineFrag.Pad(TextStyle.delimiters.unit.width)
        case Template.Delimiter(str) =>
          new LineFrag.Text(str, TextStyle.delimiters)
        case f: Template.Field =>
          childs(f.index).layout(width, forceLinear)
        case Template.Unfold(c) =>
          logicError()
        case Template.Tree(left, b1, content, sep, b2) =>
          val cons = content match {
            case Seq(f@Template.Unfold(_)) =>
              val c = dependentCast[Collection](childs(f.index))
              c.childs.map(_.layout(width - Node.DefaultIndent, forceLinear))
            case _ =>
              content.map(a => layout(a, width - Node.DefaultIndent, forceLinear))
          }
          layoutComposite(left ++ b1, cons, sep.toSeq, b2, width, forceLinear)
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
      val cs0 = if (index == -1) cs else Seq(("$type", ujson.Str(dependentCast[Type.Sum](typ).cases(index).name))) ++ cs
      ujson.Obj.from(cs0)
    }

    val template: Template = typ match {
      case record: Type.Record =>
        record.templateOrDefault
      case sum: Type.Sum =>
        sum.cases(index).templateOrDefault
    }


    override protected def doLayout(width: Float, forceLinear: Boolean): Frag = {
      layout(template, width, forceLinear)
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
        _childs.remove(last)._parent = null
        true
      } else {
        false
      }
    }

    override def save(): Value = {
      ujson.Arr.from(childs.map(_.save()))
    }


    override protected def doLayout(width: Float, forceLinear: Boolean): Frag = {
      val left = Seq(Template.Delimiter("["))
      val sep = Seq(Template.Separator(","))
      val end = Seq(Template.Delimiter("]"))
      val cs = childs.map(_.layout(width - Node.DefaultIndent, forceLinear))
      layoutComposite(left, cs, sep, end, width, forceLinear)
    }
  }

  sealed trait StringLeaf extends Node {
    override def childs: Seq[Node] = Seq.empty

    protected var buffer = ""

    override def editAppend(c: Int, small: Int): Unit = {
      buffer = buffer.take(small) + c.toChar.toString + buffer.drop(small)
    }

    override def editBackspace(small: Int): Unit = {
      buffer = buffer.take(small - 1) + buffer.drop(small)
    }

    def tryCommit(buffer: String): Boolean

    override def editCommit(): Unit = {
      if (tryCommit(buffer)) {
        buffer = ""
      }
    }


    def style: TextStyle

    override protected def doLayout(width: Float, forceLinear: Boolean): Frag = {
      new LineFrag.Text(buffer, style)
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

    def typ = dependentCast[Type.Sum](language.types(tag.index))

    override def save(): Value = ujson.Obj.from(Seq(("$choice", buffer)))

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

    override def style: TextStyle = TextStyle.choice
  }

  class Str(
      override protected var _parent: Node,
      val style: TextStyle
  ) extends StringLeaf {


  override def save(): Value = ujson.Str(buffer)

  def init(str: String): Node = {
      buffer = str
      this
    }

    override def tryCommit(buffer: String): Boolean = false
  }
}
