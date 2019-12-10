package medit.editor

import medit.draw
import medit.draw.{DrawCall, Position, TextStyle}
import medit.layout.Layout
import medit.structure.{Language, Type, TypeTag}

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
  def editCommit(): Unit = logicError()

  protected var top: Int = 0
  protected var left: Int = 0
  protected var bottom: Int = 0
  protected var right: Int = 0
  def drawTree(width: Int): draw.DrawCall
}

object Node {
  def default(parent: Node, language: Language, a: TypeTag): Node = a match {
    case TypeTag.Str => new Str(parent)
    case coll: TypeTag.Coll => new Collection(parent, language, coll)
    case n: TypeTag.Named => language.types(n.index) match {
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
    def init(childs: Seq[Node]): Unit = _childs.appendAll(childs)
    override def apply(focus: Seq[Int]): Node = if (focus.isEmpty) this else _childs(focus.head)(focus.tail)
    override def childs: Seq[Node] = _childs.toSeq

    def replace(choice: Node, s: Node) = {
      assert(choice.parent == this && s.parent == null)
      val index = childs.indexOf(choice)
      assert(index >= 0)
      _childs.update(index, s)
      s._parent = this
    }
  }

  class Structure(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Named, val index: Int) extends HaveChilds {
    def typ = language.types(tag.index)
    def childTypes = typ(index)

    override def drawTree(width: Int): DrawCall = {
      val tag = typ match {
        case Type.Record(name, fields) =>
          name
        case Type.Sum(name, cases) =>
          name + "." + cases(index).name
      }
      val style = TextStyle.default
      val measure = style.measure(tag)
      DrawCall.Translated(Position(0, 0, 0), Seq(
        DrawCall.Text(Position(measure.y, 0, 0), style, tag),
        DrawCall.Translated(
          Position(measure.y, 8, 0),
          childs.map(_.drawTree(width))
        )
      ))
    }
  }

  class Collection(
      override protected var _parent: Node,
      val language: Language, val sort: TypeTag.Coll) extends HaveChilds {
    override def tryNewChild(): Int = {
      // TODO check collection type and size limit
      _childs.append(default(this, language, sort.tt))
      _childs.size - 1
    }

    override def drawTree(width: Int): DrawCall = {
      val style = TextStyle.default
      DrawCall.Translated(Position(0, 0, 0), Seq(
        DrawCall.Text(Position(style.size, 0, 0), style, "["),
        DrawCall.Translated(
          Position(style.size, 8, 0),
          childs.map(_.drawTree(width))
        )
      ))
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

    def tryCommit(buffer: String): Boolean

    override def editCommit(): Unit = {
      if (tryCommit(buffer)) {
        buffer = ""
      }
    }

    override def drawTree(width: Int): DrawCall = {
      val style = TextStyle.default
      val text = if (buffer.isEmpty) "?" else buffer
      val measure = style.measure(text)
      DrawCall.Text(Position(measure.y, 0, 0), style, text)
    }
  }

  class Choice(
      override protected var _parent: Node,
      val language: Language, val tag: TypeTag.Named
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

  }

  class Str(
      override protected var _parent: Node
  ) extends StringBuffered with Leaf {
    override def tryCommit(buffer: String): Boolean = true
  }
}
