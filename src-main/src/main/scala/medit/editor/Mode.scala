package medit.editor


sealed trait Mode

object Mode {
  case class Frag(node: Seq[Int], pos: Int) extends Mode
  case class Edit(node: Seq[Int]) extends Mode
}
