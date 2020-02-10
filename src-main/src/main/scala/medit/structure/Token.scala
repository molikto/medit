package medit.structure

import medit.utils.pickle.{macroRW, ReadWriter => RW}

sealed trait Token {
  def str: String
}

object Token {
  @upickle.implicits.key("keyword")
  case class Keyword(str: String) extends Token
  @upickle.implicits.key("delimiter")
  case class Delimiter(str: String) extends Token
  @upickle.implicits.key("other")
  case class Matched(name: String, str: String) extends Token

  @upickle.implicits.key("unknown")
  case class Unknown(str: String) extends Token

  implicit val rw: RW[Token] = RW.merge(macroRW[Keyword], macroRW[Delimiter], macroRW[Matched], macroRW[Unknown])
}

