package medit

import scala.annotation.Annotation

object utils {

  def debug(a: => String) = println(a)

  class nullable extends Annotation

  def logicError() = throw new IllegalStateException()
  def notUsed() = throw new IllegalStateException()
}