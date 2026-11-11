
package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*
import ch.usi.si.maise.edsl.baml.model.*
import scala.util.Try

/** Trait to declare grammar rules for the BAML DSL with semantic actions.
  */
trait BAMLSyntaxWithActions(
    private var _symbolTable: SymbolTable = SymbolTable()
) extends BAMLTokensWithActions:

  /**
    * This method should only be used for testing.
    */
  def symbolTable = _symbolTable

  // Use this rule for initial space
  def InitialSpace = rule:
    WSS ~> (() => this._symbolTable = SymbolTable())

  def BAMLDSL: Rule1[BAMLFile] = rule:
    WSS ~
    zeroOrMore(TopLevelElementDeclaration) ~
    EOI ~>
    ((declarations: Seq[BAMLTopLevelElement]) => BAMLFile(declarations))

  /* Remember to refine all Rule1[Any] rules to their proper semantic model type instead of Any */

  def TopLevelElementDeclaration: Rule1[BAMLTopLevelElement] = rule:
    ClassDeclaration |
    EnumDeclaration |
    ClientDeclaration |
    FunctionDeclaration

  def ClassDeclaration: Rule1[BAMLClassDeclaration] = rule:
    "class" ~ WSS ~ Identifier ~
    "{" ~ WSS ~
    zeroOrMore(ClassField) ~
    "}" ~ WSS ~>
    (
      (name: Name, fields: Seq[BAMLClassField]) =>
        val declaration = BAMLClassDeclaration(name, fields)
        val result: Try[BAMLClassDeclaration] =
          _symbolTable.addDeclaration(declaration).map(updatedSymbolTable =>
            _symbolTable = updatedSymbolTable
            declaration
          )
        result.getOrFailParser
    )

  def ClassField: Rule1[BAMLClassField] = rule:
    Identifier ~ Type ~
    optional("@alias" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ WSS ~ ")" ~ WSS) ~
    optional("@description" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ ")" ~ WSS) ~>
    (
      (name: Name,
      typeName: BAMLType,
      optionalAlias: Option[BAMLStringValue],
      optionalDescription: Option[BAMLStringValue]) =>
        BAMLClassField(name, typeName, optionalAlias, optionalDescription)
    )

  def EnumDeclaration: Rule1[BAMLEnumDeclaration] = rule:
    "enum" ~ WSS ~ Identifier ~
    "{" ~ WSS ~
    oneOrMore(EnumValue) ~
    "}" ~ WSS ~>
    ((name: Name, values: Seq[BAMLEnumValue]) =>
      val declaration = BAMLEnumDeclaration(name, values)
      val result: Try[BAMLEnumDeclaration] =
        _symbolTable.addDeclaration(declaration).map(updatedSymbolTable =>
          _symbolTable = updatedSymbolTable
          declaration
        )
      result.getOrFailParser
    )

  def EnumValue: Rule1[BAMLEnumValue] = rule:
    Identifier ~
    optional(
      "@alias" ~ WSS ~ "(" ~ WSS ~
      StringLiteralRule ~ ")" ~ WSS
    ) ~
    optional(
      "@description" ~ WSS ~ "(" ~ WSS ~
      StringLiteralRule ~ ")" ~ WSS
    ) ~>
    ((name: Name,
      alias: Option[BAMLStringValue],
      description: Option[BAMLStringValue]) =>
      BAMLEnumValue(name, alias, description)
    )

  def Type: Rule1[BAMLType] = rule:
    // Push the matched primitive type onto the parser's value stack
    ((("bool" ~ !(CharPredicate.AlphaNum | '_') ~ WSS ~ push(BAMLPrimitiveType.BoolType)) |
    ("int" ~ !(CharPredicate.AlphaNum | '_') ~ WSS ~ push(BAMLPrimitiveType.IntType)) |
    ("float" ~ !(CharPredicate.AlphaNum | '_') ~ WSS ~ push(BAMLPrimitiveType.FloatType)) |
    ("string" ~ !(CharPredicate.AlphaNum | '_') ~ WSS ~ push(BAMLPrimitiveType.StringType)) |
    ("null" ~ !(CharPredicate.AlphaNum | '_') ~ WSS ~ push(BAMLPrimitiveType.NullType))) |
    Identifier ~> ((name: Name) => _symbolTable.resolveType(name).getOrFailParser)) ~
    zeroOrMore(capture("?" | "[]") ~ WSS) ~>
    ((typeName: BAMLType, suffixes: Seq[String]) =>
      Try {
        var currentType = typeName

        // Iterate over each suffix, wrapping the current type in an optional or list type
        // E.g. suffixes = Seq("[]", "?", "[]", "[]") and typeName = IntType
        // yields BAMLListType(BAMLListType(BAMLOptionalType(BAMLListType(IntType))))
        suffixes.foreach { suffix =>
          currentType = suffix match
            case "?"  => BAMLOptionalType(currentType)
            case "[]" => BAMLListType(currentType)
        }
        currentType
      }.getOrFailParser
    )

  def ClientDeclaration: Rule1[BAMLClientDeclaration] = rule:
    "client" ~ WSS ~ "<" ~ WSS ~ "llm" ~ WSS ~ ">" ~ WSS ~
    Identifier ~ "{" ~ WSS ~
    "provider" ~ WSS ~ ProviderRule ~
    "options" ~ WSS ~ "{" ~ WSS ~
    zeroOrMore(ClientOption) ~
    "}" ~ WSS ~
    "}" ~ WSS ~>
    ((name: Name, provider: BAMLProvider, options: Seq[BAMLClientOption]) =>
      val result = Try {
        val declaration = BAMLClientDeclaration(name, provider, options)
        _symbolTable = _symbolTable.addDeclaration(declaration).get
        declaration
      }
      result.getOrFailParser
    )

  def ClientOption: Rule1[BAMLClientOption] = rule:
    Identifier ~
    (StringLiteralRule |
    FloatLiteralRule |
    IntLiteralRule |
    BoolLiteralRule |
    EnvVariableRule) ~>
    ((key: Name, value: BAMLValue) => BAMLClientOption(key, value))

  def FunctionDeclaration: Rule1[BAMLFunctionDeclaration] = rule:
    "function" ~ WSS ~ Identifier ~
    "(" ~ WSS ~
    (Identifier ~ ":" ~ WSS ~ Type ~>
      ((name: Name, t: BAMLType) => BAMLParameter(name, t))) ~
    zeroOrMore(
      "," ~ WSS ~ Identifier ~ ":" ~ WSS ~ Type ~>
      ((name: Name, t: BAMLType) => BAMLParameter(name, t))
    ) ~>
    ((first: BAMLParameter, rest: Seq[BAMLParameter]) => first +: rest) ~
    ")" ~ WSS ~
    "->" ~ WSS ~ Type ~
    "{" ~ WSS ~
    "client" ~ WSS ~
    (Identifier ~>
      ((name: Name) => _symbolTable.resolveClient(name).getOrFailParser)) ~
    "prompt" ~ WSS ~ BlockStringRule ~
    "}" ~ WSS ~>
    ((name: Name,
      parameters: Seq[BAMLParameter],
      returnType: BAMLType,
      client: BAMLClientReference,
      prompt: BAMLPrompt) =>
      val result = Try {
        val declaration = BAMLFunctionDeclaration(name, parameters, returnType, client, prompt)
        _symbolTable = _symbolTable.addDeclaration(declaration).get
        declaration
      }
      result.getOrFailParser
    )

end BAMLSyntaxWithActions

/** The actual parser, which simply implements the constructor taking a parser
  * input (i.e., a string).
  */
case class BAMLParserWithActions(
    val input: ParserInput,
    _symbolTable: SymbolTable = SymbolTable()
) extends BAMLSyntaxWithActions(_symbolTable)
