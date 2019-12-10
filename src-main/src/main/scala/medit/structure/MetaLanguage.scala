package medit.structure

object MetaLanguage {

  val src =
    """
      |cpd typed_tag {
      | cs str_
      | cs opt_(tt: typed_tag)
      | cs arr_(tt: typed_tag)
      | cs bag_(tt: typed_tag)
      | cs named_(name: str)
      |}
      |
      |prd name_typed_tag(name: str, tag: typed_tag)
      |
      |prd case(name: str, fields: arr(name_typed_tag))
      |
      |cpd type {
      |  cs record(name: str, fields: arr(name_typed_tag))
      |  cs sum(name: str, cases: arr(case))
      |}
      |
      |root arr(type)
      |""".stripMargin

  val language = Parser.parse(src).get
}
