package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

sealed trait Template

object Template {
  sealed trait Terminal extends Template

  sealed trait Simple extends Terminal {
    def str: String
    if (str.isBlank_) throw StructureException.BlankConstants()
  }

  object Simple {
    val rw: RW[Simple] = RW.merge(
      macroRW[Keyword],
      macroRW[Delimiter],
    )
  }

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Simple

  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Simple

  @upickle.implicits.key("field")
  case class Field(name: String, template: TypeTemplate) extends Terminal with Named {
    var index = -1
  }


  @upickle.implicits.key("col")
  case class Col(b1: Simple, content: Seq[Template], breakable: Breakable) extends Template

  @upickle.implicits.key("compose")
  case class Compose(content: Seq[Template]) extends Template {
    if (content.size < 2) throw StructureException.ComposeShouldHaveMoreThanOneChildren()
  }
  object Compose {
    def apply(seq: Template*) = new Compose(seq)
  }


  implicit val rw: RW[Template] = RW.merge(
    Simple.rw,
    macroRW[Compose],
    macroRW[Col],
    macroRW[Field],
  )
}
