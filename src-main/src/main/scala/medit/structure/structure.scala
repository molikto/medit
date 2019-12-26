package medit.structure

import medit.draw.TextStyle
import medit.structure.TypeTag.Primitive
import medit.utils._
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

  case class TypeTemplateWrongType() extends StructureException

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
      if (record.fields.exists(_.name.isBlank)) throw StructureException.EmptyName()
      if (!record.fields.map(_.name).unique) throw StructureException.DuplicateName()
      record.fields.foreach(_.tag.check(types))
      record.templateOrDefault.check(record.fields, language)
    case sum: Type.Sum =>
      if (sum.cases.exists(_.name.isBlank)) throw StructureException.EmptyName()
      if (!sum.cases.map(_.name).unique) throw StructureException.DuplicateName()
      sum.cases.foreach(a => {
        if (a.fields.exists(_.name.isBlank)) throw StructureException.EmptyName()
        if (!a.fields.map(_.name).unique) throw StructureException.DuplicateName()
        a.fields.foreach(_.tag.check(types))
        a.templateOrDefault.check(a.fields, language)
      })
  }

  /** MEDIT_EXTRA_END **/
}

sealed trait Template {

  /** MEDIT_EXTRA_START **/

  import Template._
  def check(fields: Seq[NameTypeTag], language: Language): Unit = {
    val remaining = mutable.Set.from(fields.map(_.name))

    def rec(t: Template, complex: Boolean = true): Unit = {
      t match {
        case s: Template.Simple =>
          s match {
            case Template.Keyword(str) =>
              if (!complex) throw StructureException.OnlySimple()
              if (str.isBlank) throw StructureException.BlankConstants()
              language.keywords.add(str)
            case Template.Delimiter(str) =>
              if (str.isBlank) throw StructureException.BlankConstants()
              language.delimiters.add(str)
            case Template.Separator(str, _) =>
              if (str.isBlank) throw StructureException.BlankConstants()
              language.separators.add(str)
          }
        case _: Template.Pads =>
          if (!complex) throw StructureException.OnlySimple()
        case f: Template.Field =>
          if (!complex) throw StructureException.OnlySimple()
          f.index = fields.indexWhere(_.name == f.name)
          if (f.index == -1) throw StructureException.UnknownField()
          if (!remaining.remove(f.name)) throw StructureException.DuplicateFieldInTemplate()
          val fld = fields(f.index)
          f.templateOrDefault = TypeTemplate.default(f.template, fld.tag, Some(fld.name))
        case Template.Compose(content) =>
          if (!complex) throw StructureException.OnlySimple()
          content.foreach(a => rec(a))
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
  
  lazy val tokens: Seq[Template.NonStructural] = this match {
    case structural: NonStructural =>
      Seq(structural)
    case _: Pads =>
      Seq.empty
    case Tree(b1, content, sep, b2) =>
      b1.tokens ++ content.flatMap(a => Seq(sep, a)).drop(1).flatMap(_.tokens) ++ b2.tokens
    case Compose(content) =>
      content.flatMap(_.tokens)
  }

  /** MEDIT_EXTRA_END **/
}

sealed trait SeparatorOpts
object SeparatorOpts {
  @upickle.implicits.key("hide_in_line_end")
  case object HideInLineEnd extends SeparatorOpts
  implicit val rw: RW[SeparatorOpts] = RW.merge(
    macroRW[HideInLineEnd.type]
  )
}

sealed trait TypeTemplate

object TypeTemplate {
  def default(opt: Option[TypeTemplate], tag: TypeTag, fieldName: Option[String] = None): TypeTemplate = {
    opt match {
      case Some(value) =>
        value match {
          case col: Col =>
            tag match {
              case coll: TypeTag.Coll =>
                col.childOrDefault = default(col.child, coll.item)
                col
              case _ =>
                throw StructureException.TypeTemplateWrongType()
            }
          case _ =>
            value
        }
      case None =>
        tag match {
          case TypeTag.Str =>
            fieldName match {
              case Some("str") =>
                Str("const")
              case Some("name") =>
                Str("reference")
              case _ =>
                throw StructureException.UnknownTextStyle()
            }
          case coll: TypeTag.Coll =>
            val res = ColTree(
              Template.Delimiter("["),
              Template.Separator(","),
              Template.Delimiter("]")
            )
            res.childOrDefault = default(None, coll.item)
            res
          case TypeTag.Ref(name) =>
            Nominal
        }
    }
  }

  sealed trait Col extends TypeTemplate {
    val child: Option[TypeTemplate]
    var childOrDefault: TypeTemplate = null
  }

  @upickle.implicits.key("col_field")
  case class ColField(sep: Template, child: Option[TypeTemplate] = None) extends Col

  @upickle.implicits.key("col_tree")
  case class ColTree(b1: Template, sep: Template, b2: Template, child: Option[TypeTemplate] = None) extends Col

  // FIXME allow white space option
  @upickle.implicits.key("str")
  case class Str(style: String) extends TypeTemplate {
    val styleResolved = TextStyle.resolve(style) match {
      case Some(value) => value
      case None => throw StructureException.UnknownTextStyle()
    }
  }

  object Str {
    val Choice = Str("choice")
  }

  @upickle.implicits.key("nominal")
  case object Nominal extends TypeTemplate

  implicit val rw: RW[TypeTemplate] = RW.merge(
    macroRW[ColField],
    macroRW[ColTree],
    macroRW[Str],
    macroRW[Nominal.type]
  )
}

object Template {
  /** MEDIT_EXTRA_START **/
  def nameDefault(name: String, fields: Seq[NameTypeTag]): Template = {
    if (fields.isEmpty) {
      Keyword(name)
    } else {
      val namedFields = fields.size >= 100
      Compose(Seq(
        Keyword(name),
        Tree(
          Delimiter("("),
          fields.map(f => {
            val just = Template.Field(f.name, None)
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
  
  sealed trait NonStructural extends Template
  
  sealed trait Pads extends Template

  sealed trait Simple extends NonStructural {
  }

  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Simple

  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Simple

  @upickle.implicits.key("separator")
  case class Separator(str: String, opts: Set[SeparatorOpts] = Set.empty) extends Simple

  @upickle.implicits.key("pad")
  case object Pad extends Pads

  @upickle.implicits.key("field")
  case class Field(name: String, template: Option[TypeTemplate] = None) extends NonStructural {
    var index = -1
    var templateOrDefault: TypeTemplate = null
  }


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
    macroRW[Field],
    macroRW[Separator],
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

  if (types.exists(_.name.isBlank)) throw StructureException.EmptyName()
  if (!types.map(_.name).unique) throw StructureException.DuplicateName()
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


