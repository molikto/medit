package medit.structure

import medit.utils
import upickle.default.{macroRW, ReadWriter => RW}

import scala.collection.mutable

sealed trait StructureException extends Exception
object StructureException {
  case class UnfoldNotSupported() extends StructureException
  case class CannotUnfold() extends StructureException
  case class UnknownField() extends StructureException
  case class FieldNotAllowedHere() extends StructureException
  case class DuplicateFieldInTemplate() extends StructureException
  case class DuplicateName() extends StructureException

  case class UnknownReference() extends StructureException
}


sealed trait TypeTag {
  /** MEDIT_EXTRA_START **/
  def check(types: Seq[Type]): Unit = this match {
    case TypeTag.Str =>
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
  /** MEDIT_EXTRA_END **/
  @upickle.implicits.key("str")
  case object Str extends TypeTag
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
  def check(types: Seq[Type]) = this match {
    case record: Type.Record =>
      if (!utils.unique(record.fields.map(_.name))) throw StructureException.DuplicateName()
      record.fields.foreach(_.tag.check(types))
      record.template.foreach(_.check(record.fields))
      record.defaultTemplate.check(record.fields)
    case sum: Type.Sum =>
      if (!utils.unique(sum.cases.map(_.name))) throw StructureException.DuplicateName()
      sum.cases.foreach(a => {
        if (!utils.unique(a.fields.map(_.name))) throw StructureException.DuplicateName()
        a.fields.foreach(_.tag.check(types))
        a.template.foreach(_.check(a.fields))
        a.defaultTemplate.check(a.fields)
      })
  }
  /** MEDIT_EXTRA_END **/
}

sealed trait Template {
  /** MEDIT_EXTRA_START **/
  def check(fields: Seq[NameTypeTag]): Unit = {
    val remaining = mutable.Set.from(fields.map(_.name))
    def rec(t: Template, allowUnfold: Boolean = false, allowField: Boolean = true): Unit = {
      t match {
        case f: Template.FieldRef =>
          // the editor depends on these, child - field relationship should be one - one
          if (!allowField) throw StructureException.FieldNotAllowedHere()
          f match {
            case Template.Unfold(_) =>
              if (!allowUnfold) throw StructureException.UnfoldNotSupported()
            case _ =>
          }
          f.index = fields.indexWhere(_.name == f.name)
          if (f.index == -1) throw StructureException.UnknownField()
          if (!remaining.contains(f.name)) throw StructureException.DuplicateFieldInTemplate()
          remaining.remove(f.name)
        case Template.Tree(left, b1, content, sep, b2) =>
          content match {
            case Seq(f@Template.Unfold(_)) =>
              rec(f, true)
              fields(f.index).tag match {
                case coll: TypeTag.Coll =>
                case _ =>
                  throw StructureException.CannotUnfold()
              }
            case _ =>
              content.foreach(a => rec(a))
          }
          left.foreach(a => rec(a))
          b1.foreach(a => rec(a))
          // because then the field will be repeated in node
          sep.foreach(a => rec(a, allowField = false))
          b2.foreach(a => rec(a))
        case _ =>
      }
    }
    rec(this)
  }
  /** MEDIT_EXTRA_END **/
}

object Template {
  /** MEDIT_EXTRA_START **/
  def nameDefault(name: String, fields: Seq[NameTypeTag]): Template = {
    if (fields.isEmpty) {
      Keyword(name)
    } else {
      Tree(
        Seq(Keyword(name)),
        Seq(Delimiter("(")),
        fields.map(f => Template.Field(f.name)),
        Some(Separator(",")),
        Seq(Delimiter(")"))
      )
    }
  }
  sealed trait FieldRef extends Template {
    val name: String
    var index = -1
  }
  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Template
  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Template
  @upickle.implicits.key("separator")
  case class Separator(str: String) extends Template
  @upickle.implicits.key("pad")
  case object Pad extends Template
  @upickle.implicits.key("left_pad")
  case object LeftPad extends Template
  @upickle.implicits.key("right_pad")
  case object RightPad extends Template

  @upickle.implicits.key("field")
  case class Field(name: String) extends FieldRef
  @upickle.implicits.key("unfold")
  case class Unfold(name: String) extends FieldRef
  //case class Opt(before: Seq[Template], name: String, after: Seq[Template]) extends FieldRef
  @upickle.implicits.key("tree")
  case class Tree(left: Seq[Template], b1: Seq[Template], content: Seq[Template], sep: Option[Template], b2: Seq[Template]) extends Template


  implicit val rw: RW[Template] = RW.merge(macroRW[Keyword], macroRW[Delimiter], macroRW[Tree], macroRW[Field], macroRW[Separator], macroRW[Unfold], macroRW[LeftPad.type], macroRW[RightPad.type], macroRW[Pad.type])

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
  if (!utils.unique(types.map(_.name))) throw StructureException.DuplicateName()
  types.foreach(_.check(types))
  root.check(types)
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


