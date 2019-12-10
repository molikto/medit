package medit.structure

object TestLanguage {

  val src =
    """
      |
      |prd module(decs: arr(declaration))
      |
      |cpd impl_modifiers {
      |  cs impl
      |}
      |
      |prd impl_name(impl: opt(impl_modifiers), name: str)
      |
      |prd impl_term(impl: opt(impl_modifiers), term: concrete)
      |
      |prd impl_pattern(impl: opt(impl_modifiers), pattern: pattern)
      |
      |prd name_type(names: arr(impl_name), ty: concrete)
      |
      |cpd declaration_modifier {
      |  cs inductively
      |}
      |
      |cpd declaration {
      |  cs define(
      |    modifiers: bag(declaration_modifier),
      |    name: str,
      |    parameters: arr(name_type),
      |    typ: opt(concrete),
      |    term: concrete)
      |}
      |
      |cpd number_const_type {
      |  cs nat
      |  cs int
      |}
      |
      |cpd sum_modifiers {
      |  cs contextual
      |}
      |
      |
      |cpd pattern {
      |  cs atom(name: str)
      |  cs group(items: impl_pattern)
      |  cs named_group(name: str, items: impl_pattern)
      |}
      |
      |prd case(pattern: pattern, body: concrete)
      |
      |prd constructor(name: str, terms: arr(name_type))
      |
      |cpd concrete {
      |  cs axiom
      |  cs declare
      |  cs hole
      |  cs undefined
      |  cs type
      |  cs internal
      |  cs interval_zero
      |  cs interval_one
      |  cs number(body: str, typ: opt(number_const_type))
      |  cs and(left: concrete, right: concrete)
      |  cs or(left: concrete, right: concrete)
      |  cs neg(left: concrete)
      |  cs make
      |  cs up(lvl: int, term: concrete)
      |  cs reference(name: str)
      |  cs cast(left: concrete, right: concrete)
      |  cs function(domain: arr(name_type), codomain: concrete)
      |  cs record(fields: arr(name_type))
      |  cs sum(modifiers: bag(sum_modifiers), constructors: arr(constructor))
      |  cs app(left: concrete, right: arr(impl_term))
      |  cs projection(left: concrete, right: concrete)
      |  cs lambda(names: arr(impl_name), body: concrete)
      |  cs pattern_lambda(impl: opt(impl_modifiers), branches: arr(case))
      |}
      |""".stripMargin

  val language = Parser.parse(src).get
}
