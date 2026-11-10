package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*
import ch.usi.si.maise.edsl.baml.model.*

/** A parser for tokens in the BAML DSL, with semantic actions.
  */
trait BAMLTokensWithActions extends BAMLBase:

  /** Whitespace handling: every token can be optionally followed by whitespace.
    * In parboiled there is no explicit way to ignore some characters, so we
    * explicitly consume whitespace after each token.
    */
  def WS = rule:
    anyOf(" \t\n\r")

  /** Zero or more whitespaces.
    */
  def WSS = rule:
    zeroOrMore(WS)

  // Declare this to match any possible keyword for testing.
  def Keyword: Rule0 = rule:
    (
      "class" | "enum" | "int" | "string" | "client" | 
      "function" | "bool" | "float" | "provider" | "options" | 
      "env" | "prompt" | "llm" | "null" | "true" | "false" |
      "@alias" | "@description"
    ) ~ !(CharPredicate.AlphaNum | '_') ~ WSS

  // Use this to match any possible symbol for testing.
  def Symbol: Rule0 = rule:
    (
      "->" | "<" | ">" | "{" | "}" |
      "(" | ")" | "[]" | "?" | ":" | "," | "." | "@"
    ) ~ WSS

  /* Remember to refine all Rule1[Any] rules to their proper semantic model type instead of Any */

  def Identifier: Rule1[Name] = rule:
    !Keyword ~
    capture(
      (CharPredicate.Alpha | '_') ~
      zeroOrMore(CharPredicate.AlphaNum | '_')
    ) ~>
    ((s: String) => Name(s)) ~
    WSS

  def StringLiteralRule: Rule[BAMLStringValue] = rule:
    '"' ~ 
    capture(
      zeroOrMore(noneOf("\"\n\r"))
    ) ~
    '"' ~ 
    WSS ~>
    ((s: String) => BAMLStringValue(s))

  // def IntLiteralRule: Rule1[Any] = ???

  // def FloatLiteralRule: Rule1[Any] = ???

  // def BoolLiteralRule: Rule1[Any] = ???

  // def BlockStringRule: Rule1[Any] = ???

  // def EnvVariableRule: Rule1[Any] = ???

  // def ProviderRule: Rule1[Any] = ???

end BAMLTokensWithActions
