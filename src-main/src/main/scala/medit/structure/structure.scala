package medit.structure

import medit.draw.TextStyle
import medit.structure.TypeTag.Primitive
import medit.utils
import upickle.default.{macroRW, ReadWriter => RW}

import scala.collection.mutable

sealed trait StructureException extends Exception

object StructureException {

  case class CannotUnfold() extends StructureException

  case class UnknownField() extends StructureException

  case class OnlySimple() extends StructureException

  case class DuplicateFieldInTemplate() extends StructureException
  case class MissingFieldInTemplate() extends StructureException

  case class ComposeShouldHaveMoreThanOneChildren() extends StructureException

  case class DuplicateName() extends StructureException

  case class EmptyName() extends StructureException

  case class BlankConstants() extends StructureException

  case class UnknownTextStyle() extends StructureException

  case class UnknownReference() extends StructureException

}


sealed trait TypeTag {
  /** MEDIT_EXTRA_START **/
  def check(types: Seq[Type]): Unit = this match {
    case _: TypeTag.Primitive =>
    case TypeTag.Opt(tt) => tt.check(types)
    case TypeTag.Arr(tt) => tt.check(types)
    case TypeTag.Bag(tt) => tt.check(types)
    case n@TypeTag.Ref(name) =>
      n.index = types.indexWhere(_.name == name)
      if (n.index == -1) throw StructureException.UnknownReference()
  }

  /** MEDIT_EXTRA_END **/
}

object TypeTag {

  /** MEDIT_EXTRA_START **/
  sealed trait Coll extends TypeTag {
    def item: TypeTag

    def sizeLimit: Int = this match {
      case Opt(item) => 1
      case Arr(item) => Int.MaxValue
      case Bag(item) => Int.MaxValue
    }
  }

  sealed trait Primitive extends TypeTag

  /** MEDIT_EXTRA_END **/
  @upickle.implicits.key("str")
  case object Str extends Primitive {
  }

  @upickle.implicits.key("opt")
  case class Opt(item: TypeTag) extends Coll

  @upickle.implicits.key("arr")
  case class Arr(item: TypeTag) extends Coll

  @upickle.implicits.key("bag")
  case class Bag(item: TypeTag) extends Coll

  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTag {
    /** MEDIT_EXTRA_START **/
    var index = -1
    /** MEDIT_EXTRA_END **/
  }

  implicit val rw: RW[TypeTag] = RW.merge(macroRW[Str.type], macroRW[Opt], macroRW[Arr], macroRW[Bag], macroRW[Ref])
}

case class NameTypeTag(name: String, tag: TypeTag)

object NameTypeTag {
  implicit val rw: RW[NameTypeTag] = macroRW
}


case class Case(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) {
  /** MEDIT_EXTRA_START **/
  val defaultTemplate = Template.nameDefault(name, fields)

  def templateOrDefault = template.getOrElse(defaultTemplate)

  /** MEDIT_EXTRA_END **/
}

object Case {
  implicit val rw: RW[Case] = macroRW
}

sealed trait Type {
  /** MEDIT_EXTRA_START **/
  def apply(index: Int) = this match {
    case record: Type.Record =>
      assert(index == -1)
      record.fields
    case sum: Type.Sum =>
      sum.cases(index).fields
  }

  def name: String

  def check(types: Seq[Type], language: Language) = this match {
    case record: Type.Record =>
      if (record.fields.exists(_.name.isEmpty)) throw StructureException.EmptyName()
      if (!utils.unique(record.fields.map(_.name))) throw StructureException.DuplicateName()
      record.fields.foreach(_.tag.check(types))
      record.templateOrDefault.check(record.fields, language)
    case sum: Type.Sum =>
      if (sum.cases.exists(_.name.isEmpty)) throw StructureException.EmptyName()
      if (!utils.unique(sum.cases.map(_.name))) throw StructureException.DuplicateName()
      sum.cases.foreach(a => {
        if (a.fields.exists(_.name.isEmpty)) throw StructureException.EmptyName()
        if (!utils.unique(a.fields.map(_.name))) throw StructureException.DuplicateName()
        a.fields.foreach(_.tag.check(types))
        a.templateOrDefault.check(a.fields, language)
      })
  }

  /** MEDIT_EXTRA_END **/
}

sealed trait Template {

  /** MEDIT_EXTRA_START **/

  import Template._
  def removeRightPad(): Template = this match {
    case Template.Compose(Seq(a, RightPad)) => a
    case _ => this
  }

  def removeLeftPad(): Template = this match {
    case Template.Compose(Seq(LeftPad, a)) => a
    case _ => this
  }
  def check(fields: Seq[NameTypeTag], language: Language): Unit = {
    val remaining = mutable.Set.from(fields.map(_.name))

    def rec(t: Template, complex: Boolean = true): Unit = {
      t match {
        case s: Template.Simple =>
          s match {
            case Template.Keyword(str) =>
              if (str.isEmpty) throw StructureException.BlankConstants()
              language.keywords.add(str)
            case Template.Delimiter(str) =>
              if (str.isEmpty) throw StructureException.BlankConstants()
              language.delimiters.add(str)
            case Template.Separator(str) =>
              if (str.isEmpty) throw StructureException.BlankConstants()
              language.separators.add(str)
            case Template.Pad | Template.LeftPad | Template.RightPad =>
          }
        case f: Template.FieldRef =>
          if (!complex) throw StructureException.OnlySimple()
          f.index = fields.indexWhere(_.name == f.name)
          if (f.index == -1) throw StructureException.UnknownField()
          if (!remaining.remove(f.name)) throw StructureException.DuplicateFieldInTemplate()
          f match {
            case StrField(_, _) =>
            case Field(_) =>
            case ColField(_, sep) =>
              rec(sep, complex = false)
            case ColTree(b1, _, sep, b2) =>
              rec(b1, complex = false)
              rec(sep, complex = false)
              rec(b2, complex = false)
          }
        case Template.Compose(content) =>
          content.foreach(a => rec(a, complex))
        case Template.Tree(b1, content, sep, b2) =>
          if (!complex) throw StructureException.OnlySimple()
          content.foreach(a => rec(a))
          rec(b1, complex = false)
          rec(sep, complex = false)
          rec(b2, complex = false)

      }
    }
    rec(this)
    if (remaining.nonEmpty) {
      throw StructureException.MissingFieldInTemplate()
    }
  }

  /** MEDIT_EXTRA_END **/
}

object Template {
  /** MEDIT_EXTRA_START **/
  def nameDefault(name: String, fields: Seq[NameTypeTag]): Template = {
    if (fields.isEmpty) {
      Keyword(name)
    } else {
      val namedFields = fields.size >= 2
      Compose(Seq(
        Keyword(name),
        Tree(
          Delimiter("("),
          fields.map(f => {
            val just = Template.Field(f.name)
            if (namedFields) {
              Compose(Seq(Template.Delimiter(f.name), Template.Separator("="), just))
            } else {
              just
            }
          }),
          Separator(","),
          Delimiter(")")
        )
      ))
    }
  }

  sealed trait Simple extends Template {
  }

  sealed trait FieldRef extends Template {
    val name: String
    var index = -1
  }

  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Simple

  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Simple

  @upickle.implicits.key("separator")
  case class Separator(str: String) extends Simple

  @upickle.implicits.key("pad")
  case object Pad extends Simple

  @upickle.implicits.key("left_pad")
  case object LeftPad extends Simple

  @upickle.implicits.key("right_pad")
  case object RightPad extends Simple


  @upickle.implicits.key("str_field")
  case class StrField(name: String, style: String) extends FieldRef {
    val styleResolved = TextStyle.resolve(style) match {
      case Some(value) => value
      case None => throw StructureException.UnknownTextStyle()
    }
  }

  @upickle.implicits.key("field")
  case class Field(name: String) extends FieldRef

  @upickle.implicits.key("col_field")
  case class ColField(name: String, sep: Template) extends FieldRef

  @upickle.implicits.key("col_tree")
  case class ColTree(b1: Template, name: String, sep: Template, b2: Template) extends FieldRef

  @upickle.implicits.key("tree")
  case class Tree(b1: Template, content: Seq[Template], sep: Template, b2: Template) extends Template

  @upickle.implicits.key("compose")
  case class Compose(content: Seq[Template]) extends Template {
    if (content.size < 2) throw StructureException.ComposeShouldHaveMoreThanOneChildren()
  }


  implicit val rw: RW[Template] = RW.merge(
    macroRW[Compose],
    macroRW[Keyword],
    macroRW[Delimiter],
    macroRW[Tree],
    macroRW[StrField],
    macroRW[Field],
    macroRW[Separator],
    macroRW[ColTree],
    macroRW[ColField],
    macroRW[LeftPad.type],
    macroRW[RightPad.type],
    macroRW[Pad.type]
  )
}

object Type {

  @upickle.implicits.key("record")
  case class Record(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) extends Type {
    val defaultTemplate = Template.nameDefault(name, fields)

    def templateOrDefault = template.getOrElse(defaultTemplate)
  }

  @upickle.implicits.key("sum")
  case class Sum(name: String, cases: Seq[Case]) extends Type

  implicit val rw: RW[Type] = RW.merge(macroRW[Record], macroRW[Sum])
}

case class Language(types: Seq[Type], root: TypeTag) {
  /** MEDIT_EXTRA_START **/

  private[structure] val delimiters = mutable.Set[String]()
  private[structure] val separators = mutable.Set[String]()
  private[structure] val keywords = mutable.Set[String]()

  if (types.exists(_.name.isEmpty)) throw StructureException.EmptyName()
  if (!utils.unique(types.map(_.name))) throw StructureException.DuplicateName()
  types.foreach(_.check(types, this))
  root.check(types)

  println("delimiters: " + delimiters.mkString(" "))
  println("separators: " + separators.mkString(" "))
  println("keywords: " + keywords.mkString(" "))

  /** MEDIT_EXTRA_END **/
}

object Language {
  /** MEDIT_EXTRA_START **/
  def parse(str: String): Language = {
    import upickle.default._
    read[Language](str)
  }

  def serialize(language: Language): String = {
    import upickle.default._
    write(language)
  }

  /** MEDIT_EXTRA_END **/
  implicit val rw: RW[Language] = macroRW
}


