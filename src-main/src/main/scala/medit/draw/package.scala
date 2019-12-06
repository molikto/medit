package medit

package object draw {
  type Str = String

  trait Impl {
    def measure(textStyle: TextStyle, str: String): Int
  }
  var impl: Impl = null
}
