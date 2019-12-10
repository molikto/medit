package medit.structure


import java.io.File

import scala.util._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.lexical.StdLexical




/**
 *
 * VERY ULGY PARSER for now
 */
// TODO local match and local tree split syntax. give a more uniform syntax for lambda and patterns.
object Parser extends StandardTokenParsers with PackratParsers with ImplicitConversions {

  lexical.reserved.clear()
  lexical.reserved ++= List("str", "opt", "arr", "bag", "prd", "cpd", "cs", "root")
  lexical.delimiters.clear()
  lexical.delimiters ++= List("{", "}", "(", ")", ",", ":")

  def delimited[T](a: String, t: Parser[T], b: String): Parser[T] = a ~> t <~ b

  def optEmpty[T](a: Parser[Seq[T]]) = opt(a) ^^ { a => a.getOrElse(Seq.empty) }

  lazy val tag: Parser[TypeTag] =
    (keyword("str") ^^ {_ =>TypeTag.Str}) |
     (keyword("opt") ~> delimited("(", tag ,")") ^^ {a => TypeTag.Opt(a)}) |
        (keyword("arr") ~> delimited("(", tag ,")") ^^ {a => TypeTag.Arr(a)}) |
        (keyword("bag") ~> delimited("(", tag ,")") ^^ {a => TypeTag.Bag(a)}) |
  ident ^^ {a => TypeTag.Named(a)}


  lazy val nameTag: Parser[NameTypeTag] = (ident <~ ":") ~ tag ^^ {a =>
    NameTypeTag(a._1, a._2)
  }


  lazy val cas: Parser[Case] = keyword("cs") ~> ident ~ optEmpty(delimited("(", repsep(nameTag, ","),")")) ^^ { a =>
    Case(a._1, a._2)
  }

  lazy val typ: Parser[Type] = (keyword("cpd") ~> ident ~ delimited("{", rep(cas) ,"}") ^^ {a =>
    Type.Sum(a._1, a._2)
  }) | (keyword("prd") ~> ident ~ delimited("(", repsep(nameTag, ","),")") ^^ {a => {
    Type.Record(a._1, a._2)
  }})

  lazy val lang = rep(typ) ~ (keyword("root") ~> tag) ^^ { a => Language(a._1, a._2) }

  def parse(a: String) = lang(new PackratReader(new lexical.Scanner(a)))

  def src(a: File): String = scala.io.Source.fromFile(a).getLines().mkString("\n")
}

