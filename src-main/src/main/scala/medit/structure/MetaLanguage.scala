package medit.structure

import java.io.File

object MetaLanguage {

  val src = scala.io.Source.fromFile("language-meta.json").getLines().mkString("\n")
  val json = ujson.read(src)
  val language = Language.parse(src)

}
