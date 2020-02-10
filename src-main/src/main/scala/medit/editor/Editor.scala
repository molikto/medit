package medit.editor

import medit.draw._
import medit.structure.{Language, Token}
import medit.input._

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

class Editor(language: Language) {

 // 0  1  2 3
 // 'a' 'bc'
  private val tokens = mutable.ArrayBuffer.empty[Token]

  def render(canvas: Canvas, width: Int, height: Int): Unit = {
    var left = 0f
    tokens.foreach(token => {
      val (str, style) = token match {
        case Token.Keyword(str) =>
          (str, TextStyle.keyword)
        case Token.Delimiter(str) =>
          (str, TextStyle.delimiters)
        case Token.Matched(name, str) =>
          (str, language.lexer(name).styleResolved)
        case Token.Unknown(str) =>
          (str, TextStyle.default)
      }
      val measure = style.measure(str)
      canvas.draw(str, style, left, 100)
      left += measure.width
    })
  }

  def onScroll(xoffset: Double, yoffset: Double): Unit = {
  }

  // left 0, right 1, middle 2
  // press 1, release 0
  def onMouse(button: Int, press: Int, mods: Mods, xpos: Double, ypos: Double): Unit = {
  }


  def appendToken(str: String, commit: Boolean = false) = {
    val delimiterCandidates = language.delimiters.filter(_.startsWith(str)).map(s => Token.Delimiter(s))
    if (delimiterCandidates.size == 1 && delimiterCandidates.head.str == str) {
      // if only one delimiter fully matches, we commit now
      tokens.append(delimiterCandidates.head)
    } else {
      val candidates = delimiterCandidates ++
          language.keywords.filter(_.startsWith(str)).map(s => Token.Keyword(s)) ++
          language.lexers.filter(a => language.matches(a.name, str)).map(a => Token.Matched(a.name, str))
      val matches = candidates.filter(_.str == str)
      if (commit && matches.nonEmpty) {
        tokens.append(matches.head)
      } else {
        tokens.append(Token.Unknown(str))
      }
    }
  }

  def onChar(codepoint: Codepoint, mods: Mods): Unit = {
    // TODO support input space
    if (codepoint == ' '.toInt) {
      if (tokens.nonEmpty) {
        tokens.last match {
          case Token.Unknown(str) =>
            tokens.remove(tokens.size - 1)
            appendToken(str, true)
          case Token.Matched(name, str) =>
            val candidate = str + " "
            if (language.matches(name, candidate)) {
              tokens.remove(tokens.size - 1)
              tokens.append(Token.Matched(name, candidate))
            }
          case _: Token.Keyword | _: Token.Delimiter =>
        }
      }
    } else {
      onChar0(codepoint, mods)
    }
  }

  def onChar0(codepoint: Codepoint, mods: Mods) = {
    val char = Codepoint.toString(codepoint)
    if (tokens.isEmpty) {
      appendToken(char)
    } else {
      tokens.last match {
        case _: Token.Keyword | _: Token.Delimiter =>
          appendToken(char)
        case Token.Matched(name, str) =>
          val candidate = str + char
          if (language.matches(name, candidate)) {
            tokens.remove(tokens.size - 1)
            tokens.append(Token.Matched(name, candidate))
          } else {
            appendToken(char)
          }
        case Token.Unknown(str) =>
          val candidate = str + char
          tokens.remove(tokens.size - 1)
          if (language.delimiters.exists(_.startsWith(char))) {
            appendToken(str, true)
            tokens.last match {
              case _: Token.Unknown =>
                tokens.remove(tokens.size - 1)
                appendToken(candidate)
              case _ =>
                appendToken(char)
            }
          } else {
            appendToken(candidate)
          }
      }
    }
  }


  def onKey(key: Int, action: Codepoint, mods: Codepoint): Unit = {
  }
}
