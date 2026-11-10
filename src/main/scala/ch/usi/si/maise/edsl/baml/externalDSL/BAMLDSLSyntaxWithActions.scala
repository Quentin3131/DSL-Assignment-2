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

  def BAMLDSL: Rule1[BAMLFile] = ???

  /* Remember to refine all Rule1[Any] rules to their proper semantic model type instead of Any */

  def TopLevelElementDeclaration: Rule1[Any] = ???

  def ClassDeclaration: Rule1[Any] = ???

  def ClassField: Rule1[Any] = ???

  def EnumDeclaration: Rule1[BAMLEnumDeclaration] = rule:
    "enum" ~ WSS ~ Identifier ~
    "{" ~ WSS ~
    oneOrMore(EnumValue) ~
    "}" ~ WSS ~>
    ((name: Name, values: Seq[BAMLEnumValue]) =>
      val declaration = BAMLEnumDeclaration(name, values)
      _symbolTable = _symbolTable.addDeclaration(declaration).get
      declaration
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

  def Type: Rule1[Any] = ???

  def ClientDeclaration: Rule1[Any] = ???

  def ClientOption: Rule1[Any] = ???

  def FunctionDeclaration: Rule1[Any] = ???

end BAMLSyntaxWithActions

/** The actual parser, which simply implements the constructor taking a parser
  * input (i.e., a string).
  */
case class BAMLParserWithActions(val input: ParserInput, _symbolTable: SymbolTable = SymbolTable())
    extends BAMLSyntaxWithActions(_symbolTable)
