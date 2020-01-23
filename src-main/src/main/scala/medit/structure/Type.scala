package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}


case class NameTypeTag(name: String, tag: TypeTag) extends Named

object NameTypeTag {
  implicit val rw: RW[NameTypeTag] = macroRW
}
//
//sealed trait Branch {
//  def name: String
//  def collect: Seq[Branch]
//  def check(types: Seq[Type], language: Language): Unit
//}
//
//object Branch {
//  case class Trait(name: String, branches: Seq[Branch]) extends Branch {
//    override def collect: Seq[Branch] = this +: branches.flatMap(_.collect)
//
//    override def check(types: Seq[Type], language: Language): Unit = branches.foreach(_.check(types, language))
//  }
//  implicit val rw: RW[Branch] = RW.merge(macroRW[Trait], macroRW[Case])
//}


object Case {
  implicit val rw: RW[Case] = macroRW[Case]
}

case class Case(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) {
  /** MEDIT_EXTRA_START **/
  val defaultTemplate = Template.nameDefault(name, fields)

  def templateOrDefault = template.getOrElse(defaultTemplate)

  /** MEDIT_EXTRA_END **/
}



sealed trait Type extends Named {
  /** MEDIT_EXTRA_START **/
  def apply(index: Int): Seq[NameTypeTag] = this match {
    case record: Type.Record =>
      assert(index == -1)
      record.fields
    case sum: Type.Sum =>
      sum.cases(index).fields
  }

  def check(types: Seq[Type], language: Language) = this match {
    case record: Type.Record =>
      Type.checkFields(record.fields)
      record.fields.foreach(_.tag.check(types))
      record.templateOrDefault.check(record.fields, language)
    case sum: Type.Sum =>
      if (!sum.cases.map(_.name).unique) throw StructureException.DuplicateName()
      sum.cases.foreach(c => {
        val fields = c.fields
        Type.checkFields(fields)
        fields.foreach(_.tag.check(types))
        c.templateOrDefault.check(fields, language)
      })
  }

  /** MEDIT_EXTRA_END **/
}


object Type {

  def checkFields(fields: Seq[NameTypeTag]): Unit = {
    if (!fields.map(_.name).unique) throw StructureException.DuplicateName()
  }


  @upickle.implicits.key("record")
  case class Record(name: String, fields: Seq[NameTypeTag], template: Option[Template] = None) extends Type {
    val defaultTemplate = Template.nameDefault(name, fields)

    def templateOrDefault = template.getOrElse(defaultTemplate)
  }

  @upickle.implicits.key("sum")
  case class Sum(name: String, cases: Seq[Case]) extends Type {
  }

  implicit val rw: RW[Type] = RW.merge(macroRW[Record], macroRW[Sum])
}
