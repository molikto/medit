package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}


sealed trait TypeTag {
  /** MEDIT_EXTRA_START **/
  def check(types: Seq[Type]): Unit = this match {
    case _: TypeTag.Primitive =>
    case TypeTag.Opt(tt) => tt.check(types)
    case TypeTag.Arr(tt) => tt.check(types)
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
    }
  }

  sealed trait Primitive extends TypeTag

  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("str")
  case object Str extends Primitive

  @upickle.implicits.key("unit")
  case object Unit extends Primitive

  @upickle.implicits.key("opt")
  case class Opt(item: TypeTag) extends Coll

  @upickle.implicits.key("arr")
  case class Arr(item: TypeTag) extends Coll

  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTag {
    /** MEDIT_EXTRA_START **/
    var index = -1
    /** MEDIT_EXTRA_END **/
  }

  implicit val rw: RW[TypeTag] = RW.merge(macroRW[Str.type], macroRW[Opt], macroRW[Arr], macroRW[Ref])
}
