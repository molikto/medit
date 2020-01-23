package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

import scala.collection.mutable


sealed trait SeparatorOpts
object SeparatorOpts {
  @upickle.implicits.key("hide_in_line_end")
  case object HideInLineEnd extends SeparatorOpts
  implicit val rw: RW[SeparatorOpts] = RW.merge(
    macroRW[HideInLineEnd.type]
  )
}


sealed trait Template {

  /** MEDIT_EXTRA_START **/

  def check(fields: Seq[NameTypeTag], language: Language): Unit = {
    val remaining = mutable.Set.from(fields.map(_.name))

    def rec(t: Template, complex: Boolean = true): Unit = {
      t match {
        case s: Template.Simple =>
          s match {
            case Template.Keyword(str) =>
              if (!complex) throw StructureException.OnlySimple()
              language.keywords.add(str)
            case Template.Delimiter(str) =>
              language.delimiters.add(str)
            case Template.Separator(str, _) =>
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

  import Template._

  lazy val tokens: Seq[Template.Terminal] = this match {
    case structural: Terminal =>
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

  sealed trait Terminal extends Template

  sealed trait Pads extends Template

  sealed trait Simple extends Terminal {
    def str: String

    if (str.isBlank) throw StructureException.BlankConstants()
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
  case class Field(name: String, template: Option[TypeTemplate] = None) extends Terminal {
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
