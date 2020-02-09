package medit.structure

import medit.draw.TextStyle
import medit.structure.Template.Simple
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

sealed trait TypeTemplate


case class Breakable(b1: Simple, b2: Simple)

object Breakable {
  implicit val rw: RW[Breakable] = macroRW[Breakable]
  val `{}` =  Breakable(Template.Delimiter("{"), Template.Delimiter("}"))
  val `[]` =  Breakable(Template.Delimiter("["), Template.Delimiter("]"))
}

object TypeTemplate {
  @upickle.implicits.key("col")
  case class Col(child: TypeTemplate, sep: Simple, breakable: Option[Breakable]) extends TypeTemplate

  @upickle.implicits.key("str")
  case class Str(lexer: String, style: String) extends TypeTemplate {
    val styleResolved = TextStyle.resolve(style) match {
      case Some(value) => value
      case None => throw StructureException.UnknownTextStyle()
    }
  }

  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTemplate

  implicit val rw: RW[TypeTemplate] = RW.merge(
    macroRW[Col],
    macroRW[Str],
    macroRW[Ref]
  )

  def extract(temp: TypeTemplate): TypeTag = temp match {
    case TypeTemplate.Col(child, _, _) => TypeTag.Col(extract(child))
    case TypeTemplate.Str(lexer, style) => TypeTag.Str
    case TypeTemplate.Ref(name) => TypeTag.Ref(name)
  }
}
