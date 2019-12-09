# Structure for a Structural Editor


## Choose from Semantically Equivalent ASTs
when creating a formal language, AST can be defined in various ways. for example:

* modifier
  * boolean way `class Definition(isFinal: Boolean)`
  * enum way `class Definition(modifiers: Seq[Modifier])` where `enum class Modifier { case Final }`

## How Precise Should You Type the AST

structural editor cannot make ill-typed AST (ill-typed in the sense of host language. not target language, if you want a to write type-correct code only, a approach like [Hazel](https://hazel.org/))

so you should make sure that user is able to perform transitional edits, and these edits is allowed by the AST type structure

also coercion is allowed.

also after these, before AST is feed into compiler, a runtime check is performed for ill structured AST
