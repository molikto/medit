package medit.structure

import java.util.regex.Pattern

import medit.draw.TextStyle
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}

import scala.collection.mutable



case class Language(
    lexers: Seq[Lexer],
    types: Seq[Type],
    root: TypeTemplate
) {
  def matches(name: String, candidate: String): Boolean = lexer(name).regex.matcher(candidate).matches()

  def lexer(name: String): Lexer = lexers.find(_.name == name).get


  private val _delimiters = mutable.Set[String]()
  private val _keywords = mutable.Set[String]()

  if (!lexers.map(_.name).unique) throw StructureException.DuplicateName()
  if (!types.map(_.name).unique) throw StructureException.DuplicateName()

  private def check(breakable: Breakable): Unit = {
    check(breakable.b1)
    check(breakable.b2)
  }

  private def check(template: TypeTemplate): Unit = template match {
    case TypeTemplate.Col(child, sep, breakable) =>
      check(sep)
      check(breakable)
      check(child)
    case TypeTemplate.Str(lexer) =>
      if (!lexers.map(_.name).contains(lexer)) throw StructureException.UnknownReference()
    case TypeTemplate.Ref(name) =>
      if (!types.map(_.name).contains(name)) throw StructureException.UnknownReference()
  }

  private def check(template: Template): Unit = template match {
    case Template.Keyword(str) =>
      _keywords.add(str)
    case Template.Delimiter(str) =>
      _delimiters.add(str)
    case Template.Field(_, template) =>
      check(template)
    case Template.Col(b1, content, breakable) =>
      check(b1)
      content.foreach(check)
      check(breakable)
    case Template.Compose(content) =>
      content.foreach(check)
  }

  types.foreach {
    case Type.Record(_, template) => check(template)
    case Type.Sum(_, cases) =>
      cases.foreach(a => check(a.template))
  }

  val delimiters = _delimiters.toSeq.sorted
  val keywords = _keywords.toSeq.sorted
}
object Language {
  implicit val rw: RW[Language] = macroRW
}
