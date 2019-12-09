package medit

package object draw {
  type Str = String

  case class TextMeasure(my: Int, y: Int, w: Int)
  trait Impl {
    def measure(textStyle: TextStyle, str: Str): TextMeasure
  }
  var impl: Impl = null
}
