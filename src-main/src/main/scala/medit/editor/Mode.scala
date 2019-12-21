package medit.editor


sealed trait Mode

object Mode {
  case class DeleteConfirm(node: Seq[Int], previous: Mode) extends Mode
  // iterate text fragments
  case class Frag(node: Seq[Int], pos: Int) extends Mode
  // insert inside text fragments
  case class Insert(node: Seq[Int], pos: Int, editable: Boolean, small: Int, total: Int) extends Mode
}
