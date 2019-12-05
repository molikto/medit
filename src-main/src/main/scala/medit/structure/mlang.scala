package medit.structure

trait Declaration extends Structure
object Declaration {
  case class Define(name: String, parameters: Seq[], typ: Concrete, term: Concrete) extends Declaration
}

case class PatternScope(content: Seq[PatternScopeItem])
trait PatternScope extends Structure
object PatternScope {
  case class
}

trait Concrete extends Structure
object Concrete {
  case object Type extends Concrete
}

