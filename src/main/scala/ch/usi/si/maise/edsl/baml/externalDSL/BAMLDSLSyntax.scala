package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*
import ch.usi.si.maise.edsl.baml.model.*

/** Trait to declare grammar rules for the BAML DSL.
  */
trait BAMLSyntax extends BAMLTokens:

  // a BAMLDSL is a collection of declarations
  def BAMLDSL: Rule0 = rule:
    WSS ~ zeroOrMore(TopLevelElementDeclaration) ~ EOI

  // a declaration must be either a class, enum, client, or function
  def TopLevelElementDeclaration: Rule0 = rule:
    ClassDeclaration |
    EnumDeclaration |
    ClientDeclaration |
    FunctionDeclaration

  // matches a class declaration with a name and zero or more fields
  def ClassDeclaration: Rule0 = rule:
    `class` ~ Identifier ~
    `{` ~
    zeroOrMore(ClassField) ~
    `}`

  // matches a class field with a name and type
  // optionally followed by an alias and description
  def ClassField: Rule0 = rule:
    Identifier ~ Type ~
    optional(`@alias` ~ `(` ~ StringLiteralRule ~ WSS ~ `)`) ~
    optional(`@description` ~ `(` ~ StringLiteralRule ~ `)`)

  // matches an enum declaration with a name and one or more values
  def EnumDeclaration: Rule0 = rule:
    `enum` ~ Identifier ~
    `{` ~
    oneOrMore(EnumValue) ~
    `}`

  // matches an enum value with a name
  // optionally followed by an alias and description
  def EnumValue: Rule0 = rule:
    Identifier ~
    optional(`@alias` ~ `(` ~ StringLiteralRule ~ WSS ~ `)`) ~
    optional(`@description` ~ `(` ~ StringLiteralRule ~ WSS ~ `)`)

  // matches a primitive type or a custom type
  // optionally followed by any number of ? or [] suffixes
  def Type: Rule0 = rule:
    (`bool` | `int` | `float` | `string` | `null` | Identifier) ~
    zeroOrMore(("?" | "[]") ~ WSS) // semantic checks will reject invalid combinations

  // matches a client declaration with a name, provider, and options
  def ClientDeclaration: Rule0 = rule:
    `client` ~ `<` ~ `llm` ~ `>` ~
    Identifier ~ `{` ~
    `provider` ~ ProviderRule ~
    `options` ~ `{` ~
    zeroOrMore(ClientOption) ~
    `}` ~
    `}`

  // matches a client option with a name and a value
  // the value can be a string, float, integer, boolean, or environment variable
  def ClientOption: Rule0 = rule:
    Identifier ~
    (StringLiteralRule |
    FloatLiteralRule |
    IntLiteralRule |
    BoolLiteralRule |
    EnvVariableRule)

  // matches a function declaration with a name, parameters, and return type
  // followed by a body specifying a client and prompt
  def FunctionDeclaration: Rule0 = rule:
    `function` ~ Identifier ~
    `(` ~
    Identifier ~ `:` ~ Type ~
    zeroOrMore(`,` ~ Identifier ~ `:` ~ Type) ~
    `)` ~
    `->` ~ Type ~
    `{` ~
    `client` ~ Identifier ~
    `prompt` ~ BlockStringRule ~
    `}`

end BAMLSyntax

/** The actual parser, which simply implements the constructor taking a parser
  * input (i.e., a string).
  */
case class BAMLParser(val input: ParserInput) extends BAMLSyntax