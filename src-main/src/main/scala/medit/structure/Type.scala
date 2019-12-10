package medit.structure

import medit.layout.Token

sealed trait TypeTag {
  def resolve(types: Seq[Type]): Unit = this match {
    case TypeTag.Str =>
    case TypeTag.Opt(tt) => tt.resolve(types)
    case TypeTag.Arr(tt) => tt.resolve(types)
    case TypeTag.Bag(tt) => tt.resolve(types)
    case n@TypeTag.Named(name) => n.index = types.indexWhere(_.name == name)
  }
}

object TypeTag {
  case object Str extends TypeTag
  sealed trait Coll extends TypeTag {
    def tt: TypeTag
    def sizeLimit: Int
  }
  case class Opt(tt: TypeTag) extends Coll {
    override def sizeLimit: Int = 1
  }
  case class Arr(tt: TypeTag) extends Coll {
    override def sizeLimit: Int = Int.MaxValue
  }
  case class Bag(tt: TypeTag) extends Coll {
    override def sizeLimit: Int = Int.MaxValue
  }
  case class Named(name: String) extends TypeTag {
    var index = -1
  }
}

case class NameTypeTag(name: String, tag: TypeTag)

case class Case(name: String, fields: Seq[NameTypeTag])

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
  case class Record(name: String, fields: Seq[NameTypeTag]) extends Type
  case class Sum(name: String, cases: Seq[Case]) extends Type
}

case class Language(
    types: Seq[Type],
    root: TypeTag
) {
  types.foreach(_.resolve(types))
  root.resolve(types)
}


