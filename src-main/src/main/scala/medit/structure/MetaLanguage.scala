package medit.structure

import java.io.{File, PrintWriter}

object MetaLanguage {

  private val fileName = "language-meta.json"
  val src = scala.io.Source.fromFile(fileName).getLines().mkString("\n")
  val json = ujson.read(src)
  val language = Language.parse(src)

  def save(a: ujson.Value): Unit = {
    val pw = new PrintWriter(new File(fileName ))
    pw.write(a.render(2))
    pw.close()
  }
}
