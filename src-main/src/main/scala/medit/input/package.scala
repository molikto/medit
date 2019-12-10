package medit

package object input {

  object Mod {
    val SHIFT = 0x1
    val CTRL = 2
    val ALT = 4
    val SUPER = 8
  }

  type Mods = Int

  type Codepoint = Int
}
