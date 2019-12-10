package medit.structure

import upickle.default.{macroRW, ReadWriter => RW}


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

case class Case(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) {
  def templateOrDefault = template.getOrElse(Template.nameDefault(name, fields))
}
object Case {
  implicit val rw: RW[Case] = macroRW
}

sealed trait Type {
  def apply(index: Int) = this match {
    case record: Type.Record =>
      assert(index == -1)
      record.fields
    case sum: Type.Sum =>
      sum.cases(index).fields
  }

  def name: String
  def resolve(types: Seq[Type]) = this match {
    case record: Type.Record => record.fields.foreach(_.tag.resolve(types))
    case sum: Type.Sum => sum.cases.foreach(_.fields.foreach(_.tag.resolve(types)))
  }
}

sealed trait Template {
}
object Template {
  case class Keyword(name: String) extends Template
  case class Collection(b1: String, sep: String, b2: String) extends Template
  case class TightBrackets(left: Template, b1: String, content: Seq[Template], sep: String, b2: String) extends Template
  case class Field(i: Int) extends Template

  implicit val rw: RW[Template] = RW.merge(macroRW[Keyword], macroRW[Collection], macroRW[TightBrackets], macroRW[Field])

  def nameDefault(name: String, fields: Seq[NameTypeTag]): Template = {
    TightBrackets(
      Keyword(name),
      "(",
      fields.indices.map(f => Template.Field(f)),
      ",",
      ")"
    )
  }
}

object Type {
  @upickle.implicits.key("record")
  case class Record(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) extends Type {
    def templateOrDefault = template.getOrElse(Template.nameDefault(name, fields))
  }
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


