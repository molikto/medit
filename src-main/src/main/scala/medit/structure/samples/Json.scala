package medit.structure.samples

import java.util.regex.Pattern

import medit.structure._

object Json {
  // TODO support multiline constants
  val patternAny = Pattern.compile(".*")
  val language = Language(
    Seq(
      Lexer("str_const", patternAny, "const", false),
      Lexer("number_const", Pattern.compile("""-?(?=[1-9]|0(?!\d))\d+(\.\d+)?([eE][+-]?\d+)?"""), "const", true),
      Lexer("reference", patternAny, "reference", true)
    ),
    Seq(
    Type.Sum("value", Seq(
      Case("object", Template.Field("members", TypeTemplate.Col(
        TypeTemplate.Ref("member"),
        Template.Delimiter.`,`,
        Some(Breakable.`{}`)
      ))),
      Case("array", Template.Field("values", TypeTemplate.Col(
        TypeTemplate.Ref("value"),
        Template.Delimiter.`,`,
        Some(Breakable.`[]`)
      ))),
      Case("string",
        Template.Compose(Seq(
          Template.Delimiter.`"`,
          Template.Field("str", TypeTemplate.Str("str_const")),
          Template.Delimiter.`"`
        ))
      ),
      Case("number",
        Template.Field("str", TypeTemplate.Str("number_const"))
      ),
      Case("true",
        Template.Keyword("true")
      ),
      Case("false",
        Template.Keyword("false")
      ),
      Case("null",
        Template.Keyword("null")
      ),
    )),
    Type.Record("member", Template.Compose(Seq(
      Template.Field("name", TypeTemplate.Str("reference")),
      Template.Delimiter.`:`,
      Template.Field("value", TypeTemplate.Ref("value"))
    )))
  ),
    TypeTemplate.Ref("value")
  )
}
