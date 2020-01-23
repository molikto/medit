package medit.structure

import medit.draw.TextStyle
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

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
