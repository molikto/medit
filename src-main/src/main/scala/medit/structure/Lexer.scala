package medit.structure

import java.util.regex.Pattern

import medit.utils.pickle.{macroRW, ReadWriter => RW}
import medit.utils.pickle.macroRW


case class Lexer(name: String, regex: Pattern) extends Named {
}

object Lexer {
  implicit val rw: RW[Lexer] = macroRW
}
