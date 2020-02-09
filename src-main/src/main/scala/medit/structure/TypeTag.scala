package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}


sealed trait TypeTag

object TypeTag {
  sealed trait Primitive extends TypeTag

  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("str")
  case object Str extends Primitive
  @upickle.implicits.key("arr")
  case class Col(item: TypeTag) extends TypeTag

  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTag

  implicit val rw: RW[TypeTag] = RW.merge(macroRW[Str.type], macroRW[Col], macroRW[Ref])
}
