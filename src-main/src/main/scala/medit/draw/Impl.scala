package medit.draw

trait Impl {
  def measure(textStyle: TextStyle, str: String): Int
}
object Impl {
  var instance: Impl = null
}

