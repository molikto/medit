package medit

/**
 * The language definition is rather LOW-dimensional on purpose
 *
 * sort =
 * | `single` structure
 * | `cases` structures
 *
 * structure =
 * | name childs
 *
 * child =
 * | str
 * | sort
 * | sort*
 */
package object structure {

  sealed trait Sort
  object Sort {
    case class Cases(
        name: String,
        structures: Seq[Structure]
    ) extends Sort
    case class Single(structure: Structure) extends Sort
  }

  sealed trait ChildType

  object ChildType {
    case object Str extends ChildType
    case class Sort(ref: Int) extends ChildType
    case class Star(ref: Int) extends ChildType
  }

  case class Child(name: String, typ: ChildType)

  case class Structure(name: String, childs: Seq[Child])

  case class Language(
      sorts: Seq[Sort]
  )

  object Language {
    val test = {
      val str = ChildType.Str
      val expr = ChildType.Sort(0)
      val exprs = ChildType.Star(0)
      val dec = ChildType.Sort(1)
      val decs = ChildType.Star(1)
      val name = ChildType.Sort(2)
      val names = ChildType.Star(2)
      val nexpr = ChildType.Sort(3)
      val nexprs = ChildType.Sort(3)
      Language(
        Seq(
          Sort.Cases("expr", Seq(
            Structure("ref", Seq(
              Child("name", name)
            )),
            Structure("abs", Seq(
              Child("name", names),
              Child("body", expr)
            )),
            Structure("app", Seq(
              Child("left", expr),
              Child("right", exprs)
            )),
          )),
          Sort.Cases("dec", Seq(
            Structure("def", Seq(
              Child("name", name),
              Child("parameters", nexprs),
              Child("body", expr)
            )),
          )),
          Sort.Single(Structure("name", Seq(
            Child("str", str)
          ))),
          Sort.Single(Structure("nexpr", Seq(
            Child("name", name),
            Child("expr", expr)
          ))),
          Sort.Single(Structure("module", Seq(
            Child("decs", decs)
          ))),
        )
      )
    }
  }

}



