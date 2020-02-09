package medit.structure

import medit.draw.TextStyle
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

import scala.collection.mutable



case class Language(
    lexers: Seq[Lexer],
    types: Seq[Type],
    root: TypeTemplate
) {

  private val delimiters = mutable.Set[String]()
  private val keywords = mutable.Set[String]()

  if (!lexers.map(_.name).unique) throw StructureException.DuplicateName()
  if (!types.map(_.name).unique) throw StructureException.DuplicateName()

  def check(template: Template): Unit = {

  }

  types.foreach {
    case Type.Record(_, template) => check(template)
    case Type.Sum(_, cases) =>
      cases.foreach(a => check(a.template))
  }
}
object Language {
  implicit val rw: RW[Language] = macroRW
}
