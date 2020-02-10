package medit.structure


sealed trait StructureException extends Exception

object StructureException {

  case class DuplicateName() extends StructureException

  case class ComposeShouldHaveMoreThanOneChildren() extends StructureException

  case class EmptyName() extends StructureException

  case class BlankConstants() extends StructureException

  case class CannotContainSpace() extends StructureException

  case class UnknownTextStyle() extends StructureException

  case class UnknownReference() extends StructureException
}
