package medit.structure

import medit.draw.TextStyle
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

import scala.collection.mutable



case class Language(types: Seq[Type], root: TypeTag) {
  /** MEDIT_EXTRA_START **/

  private[structure] val delimiters = mutable.Set[String]()
  private[structure] val separators = mutable.Set[String]()
  private[structure] val keywords = mutable.Set[String]()

  if (!types.map(_.name).unique) throw StructureException.DuplicateName()
  types.foreach(_.check(types, this))
  root.check(types)

  println("delimiters: " + delimiters.mkString(" "))
  println("separators: " + separators.mkString(" "))
  println("keywords: " + keywords.mkString(" "))

  /** MEDIT_EXTRA_END **/
}

object Language {
  /** MEDIT_EXTRA_START **/
  def parse(str: String): Language = {
    pickle.read[Language](str)
  }

  def serialize(language: Language): String = {
    pickle.write(language)
  }

  /** MEDIT_EXTRA_END **/
  implicit val rw: RW[Language] = macroRW
}


