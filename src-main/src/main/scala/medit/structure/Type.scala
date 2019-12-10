package medit.structure

import upickle.default.{ReadWriter => RW, macroRW}


sealed trait TypeTag {
  def resolve(types: Seq[Type]): Unit = this match {
    case TypeTag.Str =>
    case TypeTag.Opt(tt) => tt.resolve(types)
    case TypeTag.Arr(tt) => tt.resolve(types)
    case TypeTag.Bag(tt) => tt.resolve(types)
    case n@TypeTag.Ref(name) => n.index = types.indexWhere(_.name == name)
  }
}

object TypeTag {
  @upickle.implicits.key("str")
  case object Str extends TypeTag
  sealed trait Coll extends TypeTag {
    def item: TypeTag
    def sizeLimit: Int
  }
  @upickle.implicits.key("opt")
  case class Opt(item: TypeTag) extends Coll {
    override def sizeLimit: Int = 1
  }
  @upickle.implicits.key("arr")
  case class Arr(item: TypeTag) extends Coll {
    override def sizeLimit: Int = Int.MaxValue
  }
  @upickle.implicits.key("bag")
  case class Bag(item: TypeTag) extends Coll {
    override def sizeLimit: Int = Int.MaxValue
  }
  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTag {
    var index = -1
  }

  implicit val rw: RW[TypeTag] = RW.merge(macroRW[Str.type], macroRW[Opt], macroRW[Arr], macroRW[Bag], macroRW[Ref])
}

case class NameTypeTag(name: String, tag: TypeTag)
object NameTypeTag {
  implicit val rw: RW[NameTypeTag] = macroRW
}

case class Case(name: String, fields: Seq[NameTypeTag])
object Case {
  implicit val rw: RW[Case] = macroRW
}

sealed trait Type {
  def apply(index: Int) = this match {
    case Type.Record(name, fields) =>
      assert(index == -1)
      fields
    case Type.Sum(name, cases) =>
      cases(index).fields
  }

  def name: String
  def resolve(types: Seq[Type]) = this match {
    case Type.Record(_, fields) => fields.foreach(_.tag.resolve(types))
    case Type.Sum(_, cases) => cases.foreach(_.fields.foreach(_.tag.resolve(types)))
  }
}

object Type {
  @upickle.implicits.key("record")
  case class Record(name: String, fields: Seq[NameTypeTag]) extends Type
  @upickle.implicits.key("sum")
  case class Sum(name: String, cases: Seq[Case]) extends Type

  implicit val rw: RW[Type] = RW.merge(macroRW[Record], macroRW[Sum])
}

case class Language(
    types: Seq[Type],
    root: TypeTag
) {
  types.foreach(_.resolve(types))
  root.resolve(types)
}

object Language {
  implicit val rw: RW[Language] = macroRW

  def parse(str: String): Language = {
    import upickle.default._
    read[Language](str)
  }

  def serialize(language: Language): String = {
    import upickle.default._
    write(language)
  }
}


