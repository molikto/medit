package medit

import java.io.{File, PrintWriter}

import scala.annotation.Annotation

object utils {

  def debug(a: => String) = println(a)
  def warn(a: => String) = println(a)

  class nullable extends Annotation

  @inline def dependentCast[T](a: Any) = a.asInstanceOf[T]
  def logicError() = throw new IllegalStateException()
  @inline def notUsed() = throw new IllegalStateException()

  def read(fileName: String): String = scala.io.Source.fromFile(fileName).getLines().mkString("\n")

  def save(a: ujson.Value, fileName: String): Unit = {
    val pw = new PrintWriter(new File(fileName ))
    pw.write(a.render(2))
    pw.close()
  }


  implicit class RichSeq[T](val a: Seq[T]) extends AnyVal {
    def unique: Boolean = a.toSet.size == a.size
  }

  implicit class RichString(val str: String) extends AnyVal {
    def isBlank: Boolean = {
      val a = str.nonEmpty && !str.exists(a => Character.isSpaceChar(a))
      !a
    }
  }
}