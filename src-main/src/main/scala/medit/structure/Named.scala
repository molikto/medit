package medit.structure

import medit.utils._

trait Named {

  def name: String

  if (name.isBlank_) throw StructureException.EmptyName()
}
