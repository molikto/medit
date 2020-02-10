package medit.structure

import medit.structure.Template.Simple
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

sealed trait Template

case class Breakable(b1: Simple, b2: Simple)

object Breakable {
  implicit val rw: RW[Breakable] = macroRW[Breakable]
  val `{}` =  Breakable(Template.Delimiter.`{`, Template.Delimiter.`}`)
  val `[]` =  Breakable(Template.Delimiter.`[`, Template.Delimiter.`]`)
}

object Template {
  sealed trait Terminal extends Template

  sealed trait Simple extends Terminal {
    def str: String

    if (str.isEmpty) throw StructureException.BlankConstants()
    if (str.exists(a => Character.isSpaceChar(a))) throw StructureException.CannotContainSpace()
  }

  object Simple {
    implicit val rw: RW[Simple] = RW.merge(

      macroRW[Keyword],
      macroRW[Delimiter],
    )
  }

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Simple

  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Simple

  object Delimiter {
    val `:` = Delimiter(":")
    val `,` = Delimiter(",")
    val `"` = Delimiter("\"")
    val `{` = Delimiter("{")
    val `}` = Delimiter("}")
    val `[` = Delimiter("[")
    val `]` = Delimiter("]")
  }

  @upickle.implicits.key("field")
  case class Field(name: String, template: TypeTemplate) extends Terminal with Named


  @upickle.implicits.key("col")
  case class Col(b1: Simple, content: Seq[Template], breakable: Breakable) extends Template

  @upickle.implicits.key("compose")
  case class Compose(content: Seq[Template]) extends Template {
    if (content.size < 2) throw StructureException.ComposeShouldHaveMoreThanOneChildren()
  }

  implicit val rw: RW[Template] = RW.merge(
    Simple.rw,
    macroRW[Compose],
    macroRW[Col],
    macroRW[Field],
  )
}
