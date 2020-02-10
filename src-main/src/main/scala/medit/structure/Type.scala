package medit.structure

import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}


case class Field(name: String, tag: TypeTag)

trait Fields {
  def template: Template


  def collect(template: Template): Seq[Field]  = template match {
    case Template.Field(name, temp) =>
      Seq(Field(name, TypeTemplate.extract(temp)))
    case Template.Col(b1, content, breakable) =>
      content.flatMap(collect)
    case Template.Compose(content) =>
      content.flatMap(collect)
    case _: Template.Simple =>
      Seq.empty
  }

  val fields = collect(template)

  if (!fields.map(_.name).unique) throw StructureException.DuplicateName()
}

case class Case(name: String, template: Template) extends Named with Fields
object Case {
  implicit val rw: RW[Case] = macroRW[Case]
}

sealed trait Type extends Named
object Type {
  @upickle.implicits.key("record")
  case class Record(name: String, template: Template) extends Type with Fields

  @upickle.implicits.key("sum")
  case class Sum(name: String, cases: Seq[Case]) extends Type {
    if (!cases.map(_.name).unique) throw StructureException.DuplicateName()
  }

  implicit val rw: RW[Type] = RW.merge(macroRW[Record], macroRW[Sum])
}





