package medit.editor

import medit.{draw, editor}
import medit.draw.{Canvas, Position, Rect, Size, TextMeasure, TextStyle}
import medit.structure.{Language, NameTypeTag, SeparatorOpts, Template, Type, TypeTag, TypeTemplate}

import scala.collection.mutable
import medit.utils._
import ujson.Value

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {

  def save(): ujson.Value

  @nullable protected var _parent: Node
  @nullable def parent: Node = _parent
  def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else logicError()
  def childs: Seq[Node]

  def invalidate(): Unit = {
    frag = null
    if (_parent != null) _parent.invalidate()
  }

  // cache invalidation: remember to invalidate parent frag when node content changed
  var frag: Frag = null
  var fragWidth: Float = -1
  var fragWidthDown: Float = -1
  var fragForceLinear: Boolean = false
  var wrapperNode = false

  def layoutForceLinear() = dependentCast[LineFrag](layout(0, 0, true))
  // calculate frags
  def layout(width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
    if (frag != null && (fragWidth != width || fragWidthDown != widthDown || fragForceLinear != forceLinear)) {
      frag = null
    }
    if (frag == null) {
      frag = doLayout(width, widthDown, forceLinear)
      if (frag.node == null) {
        wrapperNode = false
        frag.node = this
      } else {
        if (childs.size != 1) {
          logicError()
        }
        wrapperNode = true
      }
      fragWidth = width
      fragWidthDown = widthDown
      fragForceLinear = forceLinear
    }
    frag
  }

  protected def doLayout(width: Float, widthDown: Float, forceLinear: Boolean): Frag
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

    def layoutLinear(left: Template) : LineFrag =
      dependentCast[LineFrag](layout(left, 0, 0, forceLinear = true))

    @inline def layoutCompose(cs: Seq[Template], width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
      val (all, isBlock, _) = cs.foldLeft((Seq.empty[Frag], false, width - widthDown) : (Seq[Frag], Boolean, Float)) { (acc, c) =>
        val (prev, isBlock, pad) = acc
        val res = layout(c, widthDown - pad, widthDown, forceLinear)
        val next: (Seq[Frag], Boolean, Float) = res match {
          case line: LineFrag =>
            (prev :+ res, isBlock, pad + line.width)
          case a: BlockFrag =>
            (prev :+ res, true, a.lastLineWidth)
        }
        next
      }
      if (isBlock) {
        new BlockFrag.Compose(all)
      } else {
        new LineFrag.Compose(all.map(a => dependentCast[LineFrag](a)))
      }
    }

    @inline def layoutTree(left: Template, cs: Seq[Frag], sep: Template, end: Template, width: Float, forceLinear: Boolean): Frag = {
      val leftFrag = layoutLinear(left)
      val sepFrag = layoutLinear(sep)
      val endFrag = layoutLinear(end)
      if (forceLinear || !cs.exists(_.isInstanceOf[BlockFrag])
          && leftFrag.width + cs.map(a => dependentCast[LineFrag](a).width).sum + sepFrag.width * cs.size + endFrag.width <= width) {
        new LineFrag.Compose(leftFrag +: cs.flatMap(a => Seq(layoutLinear(sep), dependentCast[LineFrag](a))).drop(1) :+ endFrag)
      } else {
        new BlockFrag.Tree(Node.DefaultIndent, leftFrag, cs.zipWithIndex.map(pair => {
          if (pair._2 == cs.size - 1) {
            pair._1
          } else {
            val parent = pair._1.parent
            def matches(frag: Frag, sep: Template): Boolean = (frag, sep) match {
              case (txt: LineFrag.Text, Template.Separator(str, opts)) =>
                txt.text == str &&
                    txt.hideInLineEnd == opts.contains(SeparatorOpts.HideInLineEnd) &&
                    txt.style == TextStyle.delimiters
              case _ => false
            }
            if (parent != null && parent.frags.size == 2 && parent.frags(0) == pair._1 && matches(parent.frags(1), sep)) {
              pair._1.parent
            } else {
              pair._1 match {
                case frag: BlockFrag =>
                  new BlockFrag.Compose(Seq(frag, layoutLinear(sep)))
                case frag: LineFrag =>
                  new LineFrag.Compose(Seq(frag, layoutLinear(sep)))
              }
            }
          }
        }), endFrag)
      }
    }

    def assignTypeTemplate(f: TypeTemplate, child: Node): Unit = {
      f match {
        case col: TypeTemplate.Col =>
          dependentCast[Node.Collection](child).template = col
        case s: TypeTemplate.Str =>
          dependentCast[Node.Str](child).template = s
        case TypeTemplate.Nominal =>
      }
    }

    def layout(left: Template, width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
      left match {
        case Template.Keyword(name) =>
          new LineFrag.Text(name, TextStyle.keyword)
        case Template.Separator(name, opts) =>
          val style = TextStyle.delimiters
          val unit = style.unit.width / 3F
          // we cannot use LineFrag.Pad here, as they create extra insertion point
          new LineFrag.Text(
            name,
            TextStyle.delimiters,
            hideInLineEnd = opts.contains(SeparatorOpts.HideInLineEnd),
            pad = unit
          )
        case Template.Pad =>
          new LineFrag.Pad(TextStyle.delimiters.unit.width * 0.75F)
        case Template.Delimiter(str) =>
          new LineFrag.Text(str, TextStyle.delimiters)
        case Template.Compose(content) =>
          layoutCompose(content, width, widthDown, forceLinear)
        case f: Template.Field =>
          val child = childs(f.index)
          assignTypeTemplate(f.templateOrDefault, child)
          child.layout(width, widthDown, forceLinear)
        case Template.Tree(b1, content, sep, b2) =>
          // when measuring childs, we assume that the size avaliable is using widthDown
          val wd = widthDown - Node.DefaultIndent
          val cons = content.map(a => layout(a, wd, wd, forceLinear))
          layoutTree(b1, cons, sep, b2, width, forceLinear)
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
        if (index == -1) {
          logicError()
        }
        sum.cases(index).templateOrDefault
    }


    override protected def doLayout(width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
      layout(template, width, widthDown, forceLinear)
    }
  }

  class Collection(
      override protected var _parent: Node,
      val language: Language, val sort: TypeTag.Coll) extends HaveChilds {

    var template: TypeTemplate.Col= null

    override protected def doLayout(width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
      template match {
        case f@TypeTemplate.ColField(sep, _) =>
          new LineFrag.Compose(childs.map( a=> {
            assignTypeTemplate(f.childOrDefault, a)
            a.layoutForceLinear()
          }).flatMap(a => Seq(layoutLinear(sep), a)).drop(1))
        case f@TypeTemplate.ColTree(b1, sep, b2, _) =>
          val wd = widthDown - Node.DefaultIndent
          val cons = childs.map(a => {
            assignTypeTemplate(f.childOrDefault, a)
            a.layout(wd, wd, forceLinear)
          })
          if (cons.isEmpty) {
            // FIXME we assume that these are optional now
            new LineFrag.Compose(Seq.empty)
          } else {
            layoutTree(b1, cons, sep, b2, width, forceLinear)
          }
      }
    }

    @nullable def insertNewAt(index: Int): Node = {
      if (_childs.size < sort.sizeLimit) {
        invalidate()
        val c = default(this, language, sort.item)
        _childs.insert(index, c)
        _childs.size - 1
        c
      } else {
        null
      }
    }
//
//
//    override def tryDelete(last: Int): Boolean = {
//      if (last < childs.size) {
//        _childs.remove(last)._parent = null
//        true
//      } else {
//        false
//      }
//    }

    override def save(): Value = {
      ujson.Arr.from(childs.map(_.save()))
    }
  }

  sealed trait EditableLeaf extends Node {
    override def childs: Seq[Node] = Seq.empty

    def text: String = buffer

    protected var buffer = ""

    def appendEdit(c: Int, small: Int): Unit = {
      invalidate()
      buffer = buffer.take(small) + c.toChar.toString + buffer.drop(small)
    }

    def backspaceEdit(small: Int): Unit = {
      invalidate()
      buffer = buffer.take(small - 1) + buffer.drop(small)
    }

    def template: TypeTemplate.Str

    override protected def doLayout(width: Float, widthDown: Float, forceLinear: Boolean): Frag = {
      new LineFrag.Text(buffer, template.styleResolved, emptyAsQuestionMark = true)
    }
  }

  class Choice(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Ref
  ) extends EditableLeaf {
    def init(c: String): _root_.medit.editor.Node = {
      buffer = c
      this
    }

    def typ = dependentCast[Type.Sum](language.types(tag.index))

    override def save(): Value = ujson.Obj.from(Seq(("$choice", buffer)))

    def tryCommit(): Boolean = {
      typ.cases.indexWhere(_.name == buffer) match {
        case -1 => false
        case i =>
          invalidate()
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

    override def template: TypeTemplate.Str = TypeTemplate.Str.Choice
  }

  class Str(
      override protected var _parent: Node
  ) extends EditableLeaf {
    // this is modified when layout, not ideal but ok
    var template: TypeTemplate.Str = null
    override def save(): Value = ujson.Str(buffer)

    def init(str: String): Node = {
      buffer = str
      this
    }
  }
}
