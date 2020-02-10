package medit.editor

sealed trait FragPart {
  def node: Node
  def lastOfNode = false
}

sealed trait PartOfStructure extends FragPart
object PartOfStructure {
  case class ConstantAt(node: Node.Structure, index: Int, size: Int) extends PartOfStructure {
    override def lastOfNode = index == size - 1
  }
}

sealed trait PartOfCollection extends FragPart
object PartOfCollection {
  case class B1(node: Node.Collection) extends PartOfCollection
  case class SeparatorAt(node: Node.Collection, index: Int) extends PartOfCollection
  case class B2(node: Node.Collection) extends PartOfCollection {
    override def lastOfNode: Boolean = true
  }
}

case class JustStringNode(node: Node.EditableLeaf) extends FragPart
