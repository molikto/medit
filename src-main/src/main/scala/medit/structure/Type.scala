package medit.structure

import upickle.default.{macroRW, ReadWriter => RW}


sealed trait TypeTag {
  /** MEDIT_EXTRA_START **/
  def resolve(types: Seq[Type]): Unit = this match {
    case TypeTag.Str =>
    case TypeTag.Opt(tt) => tt.resolve(types)
    case TypeTag.Arr(tt) => tt.resolve(types)
    case TypeTag.Bag(tt) => tt.resolve(types)
    case n@TypeTag.Ref(name) =>
      n.index = types.indexWhere(_.name == name)
      assert(n.index != -1)
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
      case Bag(item) => Int.MaxValue
    }
  }
  /** MEDIT_EXTRA_END **/
  @upickle.implicits.key("str")
  case object Str extends TypeTag
  @upickle.implicits.key("opt")
  case class Opt(item: TypeTag) extends Coll
  @upickle.implicits.key("arr")
  case class Arr(item: TypeTag) extends Coll
  @upickle.implicits.key("bag")
  case class Bag(item: TypeTag) extends Coll
  @upickle.implicits.key("ref")
  case class Ref(name: String) extends TypeTag {
    /** MEDIT_EXTRA_START **/
    var index = -1
    /** MEDIT_EXTRA_END **/
  }
  implicit val rw: RW[TypeTag] = RW.merge(macroRW[Str.type], macroRW[Opt], macroRW[Arr], macroRW[Bag], macroRW[Ref])
}

case class NameTypeTag(name: String, tag: TypeTag)
object NameTypeTag {
  implicit val rw: RW[NameTypeTag] = macroRW
}

case class Case(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) {
  /** MEDIT_EXTRA_START **/
  val defaultTemplate = Template.nameDefault(name, fields)
  def templateOrDefault = template.getOrElse(defaultTemplate)
  /** MEDIT_EXTRA_END **/
}
object Case {
  implicit val rw: RW[Case] = macroRW
}

sealed trait Type {
  /** MEDIT_EXTRA_START **/
  def apply(index: Int) = this match {
    case record: Type.Record =>
      assert(index == -1)
      record.fields
    case sum: Type.Sum =>
      sum.cases(index).fields
  }

  def name: String
  def resolve(types: Seq[Type]) = this match {
    case record: Type.Record =>
      record.fields.foreach(_.tag.resolve(types))
      record.template.foreach(_.resolve(record.fields))
      record.defaultTemplate.resolve(record.fields)
    case sum: Type.Sum =>
      sum.cases.foreach(a => {
        a.fields.foreach(_.tag.resolve(types))
        a.template.foreach(_.resolve(a.fields))
        a.defaultTemplate.resolve(a.fields)
      })
  }
  /** MEDIT_EXTRA_END **/
}

sealed trait Template {
  /** MEDIT_EXTRA_START **/
  def resolve(fields: Seq[NameTypeTag]): Unit = this match {
    case f@Template.Field(name) =>
      f.index = fields.indexWhere(_.name == name)
      assert(f.index != -1)
    case Template.Tree(left, b1, content, sep, b2) =>
      left.foreach(_.resolve(fields))
      b1.foreach(_.resolve(fields))
      sep.foreach(_.resolve(fields))
      b2.foreach(_.resolve(fields))
      content.foreach(_.resolve(fields))
    case Template.Unfold(content) =>
      content.resolve(fields)
    case _ =>
  }
  /** MEDIT_EXTRA_END **/
}

object Template {
  /** MEDIT_EXTRA_START **/
  def nameDefault(name: String, fields: Seq[NameTypeTag]): Template = {
    if (fields.isEmpty) {
      Keyword(name)
    } else {
      Tree(
        Seq(Keyword(name)),
        Seq(Delimiter("(")),
        fields.map(f => Template.Field(f.name)),
        Some(Separator(",")),
        Seq(Delimiter(")"))
      )
    }
  }
  /** MEDIT_EXTRA_END **/

  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Template
  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Template
  @upickle.implicits.key("separator")
  case class Separator(str: String) extends Template
  @upickle.implicits.key("left_pad")
  case object LeftPad extends Template
  @upickle.implicits.key("right_pad")
  case object RightPad extends Template
  @upickle.implicits.key("field")
  case class Field(name: String) extends Template {
    /** MEDIT_EXTRA_START **/
    var index = -1
    /** MEDIT_EXTRA_END **/
  }
  case object Nil extends Template
  @upickle.implicits.key("unfold")
  case class Unfold(content: Template) extends Template
  @upickle.implicits.key("tree")
  case class Tree(left: Seq[Template], b1: Seq[Template], content: Seq[Template], sep: Option[Template], b2: Seq[Template]) extends Template


  implicit val rw: RW[Template] = RW.merge(macroRW[Keyword], macroRW[Delimiter], macroRW[Tree], macroRW[Field], macroRW[Separator], macroRW[Unfold], macroRW[LeftPad.type], macroRW[RightPad.type])

}

object Type {
  @upickle.implicits.key("record")
  case class Record(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) extends Type {
    val defaultTemplate = Template.nameDefault(name, fields)
    def templateOrDefault = template.getOrElse(defaultTemplate)
  }
  @upickle.implicits.key("sum")
  case class Sum(name: String, cases: Seq[Case]) extends Type
  implicit val rw: RW[Type] = RW.merge(macroRW[Record], macroRW[Sum])
}

case class Language(types: Seq[Type], root: TypeTag) {
  /** MEDIT_EXTRA_START **/
  types.foreach(_.resolve(types))
  root.resolve(types)
  /** MEDIT_EXTRA_END **/
}

object Language {
  /** MEDIT_EXTRA_START **/
  def parse(str: String): Language = {
    import upickle.default._
    read[Language](str)
  }

  def serialize(language: Language): String = {
    import upickle.default._
    write(language)
  }
  /** MEDIT_EXTRA_END **/
  implicit val rw: RW[Language] = macroRW
}


