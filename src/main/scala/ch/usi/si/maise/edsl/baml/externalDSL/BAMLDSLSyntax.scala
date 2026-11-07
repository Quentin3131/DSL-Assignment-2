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


  def ClassDeclaration: Rule0 = rule:
    "class" ~ WSS ~ Identifier ~
    "{" ~ WSS ~
    zeroOrMore(ClassField) ~
    "}" ~ WSS

  // matches a class field with a name and type
  // optionally followed by an alias and description
  def ClassField: Rule0 = rule:
    Identifier ~ Type ~
    optional("@alias" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ WSS ~ ")" ~ WSS) ~
    optional("@description" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ ")" ~ WSS)

  
  def EnumDeclaration: Rule0 = rule:
    "enum" ~ WSS ~ Identifier ~
    "{" ~ WSS ~
    oneOrMore(EnumValue) ~
    "}" ~ WSS

  def EnumValue: Rule0 = rule:
    Identifier ~
    optional("@alias" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ WSS ~ ")" ~ WSS) ~
    optional("@description" ~ WSS ~ "(" ~ WSS ~ StringLiteralRule ~ WSS ~ ")" ~ WSS)


  // matches a primitive type or a custom type
  // optionally followed by any number of ? or [] suffixes
  
  def Type: Rule0 = rule:
    (("bool" | "int" | "float" | "string" | "null") ~
    !(CharPredicate.AlphaNum | '_') ~ WSS |
    Identifier) ~
    zeroOrMore(("?" | "[]") ~ WSS) // semantic checks will reject invalid combinations


  
  def ClientDeclaration: Rule0 = rule:
    "client" ~ WSS ~ "<" ~ WSS ~ "llm" ~ WSS ~ ">" ~ WSS ~
    Identifier ~ "{" ~ WSS ~
    "provider" ~ WSS ~ ProviderRule ~ // since providers can be a string or an Identifier
    "options" ~ WSS ~ "{" ~ WSS ~
    zeroOrMore(ClientOption) ~
    "}" ~ WSS ~
    "}" ~ WSS


  
  def ClientOption: Rule0 = rule:
    Identifier ~
    (StringLiteralRule |
    FloatLiteralRule |
    IntLiteralRule |
    BoolLiteralRule |
    EnvVariableRule)


  
  def FunctionDeclaration: Rule0 = rule:
    "function" ~ WSS ~ Identifier ~ 
    "(" ~ WSS ~
    Identifier ~ ":" ~ WSS ~ Type ~
    zeroOrMore("," ~ WSS ~ Identifier ~ ":" ~ WSS ~ Type) ~
    ")" ~ WSS ~
    "->" ~ WSS ~ Type ~
    "{" ~ WSS ~
    "client" ~ WSS ~ Identifier ~
    "prompt" ~ WSS ~ BlockStringRule ~
    "}" ~ WSS


end BAMLSyntax

/** The actual parser, which simply implements the constructor taking a parser
  * input (i.e., a string).
  */
case class BAMLParser(val input: ParserInput) extends BAMLSyntax
