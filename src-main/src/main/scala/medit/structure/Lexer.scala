package medit.structure

import java.util.regex.Pattern

import medit.draw.TextStyle
import medit.utils._
import medit.utils.pickle.{macroRW, ReadWriter => RW}


case class Lexer(name: String, regex: Pattern, style: String, breakAtDelimiters: Boolean) extends Named {
  val styleResolved = TextStyle.resolve(style) match {
    case Some(value) => value
    case None => throw StructureException.UnknownTextStyle()
  }
}

object Lexer {

  implicit val patternRw: RW[Pattern] = pickle.readwriter[String].bimap[Pattern](p => p.pattern(), str => Pattern.compile(str))
  implicit val rw: RW[Lexer] = macroRW
}
