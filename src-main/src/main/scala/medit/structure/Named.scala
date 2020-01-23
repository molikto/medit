package medit.structure

trait Named {

  def name: String

  if (name.isBlank) throw StructureException.EmptyName()
}
