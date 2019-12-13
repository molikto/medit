package medit

package object draw {

  trait Impl {
    def measure(textStyle: TextStyle, str: String): TextMeasure
  }
  trait Canvas {
    def draw(text: String, style: TextStyle, left: Float, top: Float): Unit
    def draw(rect: Rect, style: ShapeStyle): Unit
    def save(): Unit
    def restore(): Unit
    def translate(x: Float, y: Float): Unit
  }
  var impl: Impl = null
}
