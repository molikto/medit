package medit.editor

import medit.draw
import medit.draw.{DrawCall, Position, Rect, TextStyle}
import medit.layout.Layout
import medit.structure.{Data, Language, Type, TypeTag}

import scala.collection.mutable
import medit.utils._

// node is the mutable structure, also they have caches for most ui stuff, and they are recalculated at times
sealed trait Node {


  //
//  private var _parent: Node = null // root node has empty parent
//  private var _layout: Layout = null
//  private var _draw: DrawCall = null
//  def layout: Layout = ???
//
//  def draw(width: Int): DrawCall = _draw
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
  protected var top: Int = 0
  protected var left: Int = 0
  protected var width: Int = 0
  protected var height: Int = 0

  def rect: Rect = Rect(top, left, width, height)
  def drawTree(left: Int, top: Int, width: Int): draw.DrawCall
}

object Node {
  def create(parent: Node, language: Language, a: TypeTag, data: ujson.Value): Node = a match {
    case TypeTag.Str => new Str(parent).init(data.str)
    case coll: TypeTag.Coll =>
      val col = new Collection(parent, language, coll)
      col.init(data.arr.toSeq.map(a => create(col, language, coll.item, a)))
      col
    case n: TypeTag.Ref => language.types(n.index) match {
      case Type.Record(name, fields) =>
        val s = new Structure(parent, language, n, -1)
        val obj = data.obj
        s.init(fields.map(f => create(s, language, f.tag, obj(f.name))))
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
      case Type.Record(name, fields) =>
        val s = new Structure(parent, language, n, -1)
        s.init(fields.map(f => default(s, language, f.tag)))
        s
      case s: Type.Sum =>
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

    def replace(choice: Node, s: Node) = {
      assert(choice.parent == this && s.parent == null)
      val index = childs.indexOf(choice)
      assert(index >= 0)
      _childs.update(index, s)
      s._parent = this
    }

    def drawTree(left: Int, top: Int, width: Int, tag: String, style: TextStyle): draw.DrawCall = {
      this.left = left
      this.top = top
      val measure = style.measure(tag)
      val (cs, ht) = childs.foldLeft((Seq.empty[DrawCall], top + measure.y)) { (acc, n) =>
        val c = n.drawTree(left + 8, acc._2, width)
        (acc._1 :+ c, n.top + n.height)
      }
      this.height = ht - top
      DrawCall.Group(Seq(
        DrawCall.Text(Position(top + measure.y, left, 0), style, tag),
        DrawCall.Group(cs)
      ))
    }
  }

  class Structure(
                   override protected var _parent: Node,
                   val language: Language, val tag: TypeTag.Ref, val index: Int) extends HaveChilds {
    def typ = language.types(tag.index)
    def childTypes = typ(index)

    override def drawTree(left: Int, top: Int, width: Int): draw.DrawCall = {
      val tag = typ match {
        case Type.Record(name, fields) =>
          name
        case Type.Sum(name, cases) =>
          name + "." + cases(index).name
      }
      drawTree(left, top, width, tag, TextStyle.keyword)
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

    override def drawTree(left: Int, top: Int, width: Int): DrawCall = {
      drawTree(left, top, width, "[", TextStyle.delimiters)
    }
  }

  sealed trait Leaf extends Node {
    override def childs: Seq[Node] = Seq.empty
  }

  sealed trait StringBuffered extends Node {
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

    def drawTree(left: Int, top: Int, width: Int, style: TextStyle): DrawCall = {
      this.left = left
      this.top = top
      val tag = if (buffer.isEmpty) "?" else buffer
      val measure = style.measure(tag)
      this.height = measure.y
      DrawCall.Text(Position(top + measure.y, left, 0), style, tag)
    }
  }

  class Choice(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Ref
  ) extends StringBuffered with Leaf {
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

    override def drawTree(left: Int, top: Int, width: Int): DrawCall = {
      drawTree(left, top, width, TextStyle.error)
    }
  }

  class Str(
      override protected var _parent: Node
  ) extends StringBuffered with Leaf {
    def init(str: String): Node = {
      buffer = str
      this
    }

    override def tryCommit(buffer: String): Boolean = false

    override def drawTree(left: Int, top: Int, width: Int): DrawCall = {
      drawTree(left, top, width, TextStyle.reference)
    }
  }
}
