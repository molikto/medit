package medit.editor


sealed trait Mode

object Mode {
  case class Frag(node: Seq[Int], pos: Int) extends Mode
  case class Insert(node: Seq[Int], pos: Int, editable: Boolean, small: Int, total: Int) extends Mode
}
