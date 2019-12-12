package medit

import scala.annotation.Annotation

object utils {

  def debug(a: => String) = println(a)

  class nullable extends Annotation

  @inline def dependentCast[T](a: Any) = a.asInstanceOf[T]
  def logicError() = throw new IllegalStateException()
  @inline def notUsed() = throw new IllegalStateException()
}