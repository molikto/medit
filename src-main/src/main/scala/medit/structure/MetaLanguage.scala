package medit.structure

import java.io.File

object MetaLanguage {

  val language = Language.parse(scala.io.Source.fromFile("language-meta.json").getLines().mkString("\n"))

}
