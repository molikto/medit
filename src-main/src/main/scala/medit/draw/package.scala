package medit

package object draw {
  type Str = String




  trait Impl {
    def measure(textStyle: TextStyle, str: Str): TextMeasure
  }
  var impl: Impl = null
}
