
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

  def TopLevelElementDeclaration: Rule1[BAMLTopLevelElement] = rule:
    ClassDeclaration |
    EnumDeclaration |
    ClientDeclaration |
    FunctionDeclaration

  def ClassDeclaration: Rule1[BAMLClassDeclaration] = rule:
    `class` ~ Identifier ~
    `{` ~
    zeroOrMore(ClassField) ~
    `}` ~>
    (
      (name: Name, fields: Seq[BAMLClassField]) =>
        val declaration = BAMLClassDeclaration(name, fields)

        val trySymbolTable: Try[SymbolTable] = Try:
          // ensure the number of distinct names is the same as the number of fields
          if fields.map(_.name).distinct.size != fields.size then
            throw IllegalArgumentException(
              s"Duplicate field in class ${name.rep}"
            )
          _symbolTable.addDeclaration(declaration).get

        val tryDeclaration = trySymbolTable.map(updatedSymbolTable => // only runs if trySymbolTable is Success
          _symbolTable = updatedSymbolTable // update symbol table
          declaration // return the declaration
        )

        tryDeclaration.getOrFailParser // return the declaration or fail the parser if an error occurred

    )

  def ClassField: Rule1[BAMLClassField] = rule:
    Identifier ~ Type ~
    optional(`@alias` ~ `(` ~ StringLiteralRule ~ `)`) ~
    optional(`@description` ~ `(` ~ StringLiteralRule ~ `)`) ~>
    (
      (name: Name,
      typeName: BAMLType,
      optionalAlias: Option[BAMLStringValue],
      optionalDescription: Option[BAMLStringValue]) =>
        BAMLClassField(name, typeName, optionalAlias, optionalDescription)
    )

  def EnumDeclaration: Rule1[BAMLEnumDeclaration] = rule:
    `enum` ~ Identifier ~
    `{` ~
    oneOrMore(EnumValue) ~
    `}` ~>
    (
      (name: Name, values: Seq[BAMLEnumValue]) =>
        val declaration = BAMLEnumDeclaration(name, values)

        val trySymbolTable: Try[SymbolTable] = Try:
          // ensure the number of distinct names is the same as the number of fields
          if values.map(_.name).distinct.size != values.size then
            throw IllegalArgumentException(
              s"Duplicate value in enum ${name.rep}"
            )
          _symbolTable.addDeclaration(declaration).get

        val tryDeclaration = trySymbolTable.map(updatedSymbolTable =>
          _symbolTable = updatedSymbolTable
          declaration
        )
        tryDeclaration.getOrFailParser
    )

  def EnumValue: Rule1[BAMLEnumValue] = rule:
    Identifier ~
    optional(
      `@alias` ~ `(` ~
      StringLiteralRule ~ `)`
    ) ~
    optional(
      `@description` ~ `(` ~
      StringLiteralRule ~ `)`
    ) ~>
    ((name: Name,
      alias: Option[BAMLStringValue],
      description: Option[BAMLStringValue]) =>
      BAMLEnumValue(name, alias, description)
    )

  def Type: Rule1[BAMLType] = rule:
    (
      (`bool` ~ push(BAMLPrimitiveType.BoolType)) |
      (`int` ~ push(BAMLPrimitiveType.IntType)) |
      (`float` ~ push(BAMLPrimitiveType.FloatType)) |
      (`string` ~ push(BAMLPrimitiveType.StringType)) |
      (`null` ~ push(BAMLPrimitiveType.NullType)) |
      (Identifier ~>
        ((name: Name) => _symbolTable.resolveType(name).getOrFailParser))
    ) ~
    zeroOrMore(capture("?" | "[]") ~ WSS) ~>
    ((typeName: BAMLType, suffixes: Seq[String]) =>
      Try {
        var currentType = typeName

        // Apply suffixes in order.
        // For example: int?[] becomes
        // BAMLListType(BAMLOptionalType(IntType)).
        suffixes.foreach { suffix =>
          currentType = suffix match
            case "?"  => BAMLOptionalType(currentType)
            case "[]" => BAMLListType(currentType)
        }
        currentType
      }.getOrFailParser
    )

  def ClientDeclaration: Rule1[BAMLClientDeclaration] = rule:
    `client` ~ `<` ~ `llm` ~ `>` ~
    Identifier ~ `{` ~
    `provider` ~ ProviderRule ~
    `options` ~ `{` ~
    zeroOrMore(ClientOption) ~
    `}` ~
    `}` ~>
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
    `function` ~ Identifier ~
    `(` ~
    (Identifier ~ `:` ~ Type ~>
      ((name: Name, t: BAMLType) => BAMLParameter(name, t))) ~
    zeroOrMore(
      `,` ~ Identifier ~ `:` ~ Type ~>
      ((name: Name, t: BAMLType) => BAMLParameter(name, t))
    ) ~>
    ((first: BAMLParameter, rest: Seq[BAMLParameter]) => first +: rest) ~
    `)` ~
    `->` ~ Type ~
    `{` ~
    `client` ~
    (Identifier ~>
      ((name: Name) => _symbolTable.resolveClient(name).getOrFailParser)) ~
    `prompt` ~ BlockStringRule ~
    `}` ~>
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
