package medit.editor


sealed trait Mode

object Mode {
  case class DeleteConfirm(node: Seq[Int], previous: Mode) extends Mode
  // this pos is line break independent
  case class Insert(index: Index) extends Mode
}
