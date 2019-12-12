package medit

package object draw {

  trait Impl {
    def measure(textStyle: TextStyle, str: String): TextMeasure
  }
  var impl: Impl = null
}
