package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*

/** A parser for tokens in the BAML DSL, without semantic actions.
  */
trait BAMLTokens extends BAMLBase:

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

  // is not a keyword
  // starts with an alpha character or _
  // ends with any number of alphanumeric chars or _
  def Identifier: Rule0 = rule:
    !Keyword ~
    (CharPredicate.Alpha | '_') ~
    zeroOrMore(CharPredicate.AlphaNum | '_') ~
    WSS

  // Opening double quote, followed by zero or more characters
  // except double quotes, \n or \r, followed by a closing double quote.
  def StringLiteralRule: Rule0 = rule:
    '"' ~ zeroOrMore(noneOf("\"\n\r")) ~ '"' ~ WSS

  // starts with '-' or no symbol
  // if the first number is 0, then no numbers can follow
  // if the first number isn't 0, then zero or more digits can follow
  def IntLiteralRule: Rule0 = rule:
    optional('-') ~
    ('0' | (anyOf("123456789") ~ zeroOrMore(CharPredicate.Digit))) ~
    WSS

  // starts with '-' or no symbol
  // if the first number is 0, the '.' must follow
  // if the first number isn't 0, then any number of digits can follow before the '.'
  // one or more digits must follow after the '.'
  def FloatLiteralRule: Rule0 = rule:
    optional('-') ~
    ('0' | (anyOf("123456789") ~ zeroOrMore(CharPredicate.Digit))) ~
    '.' ~
    oneOrMore(CharPredicate.Digit) ~
    WSS

  // true or false followed by whitespace
  def BoolLiteralRule: Rule0 = rule:
    ("true" | "false") ~
    !(CharPredicate.AlphaNum | '_') ~
    WSS

  // starts with #"
  // followed by zero or more characters until "# is reached
  // ends with "#
  // followed by whitespace
  def BlockStringRule: Rule0 = rule:
    "#\"" ~
    zeroOrMore(!"\"#" ~ ANY) ~
    "\"#" ~
    WSS

  // env. followed by an Identifier
  // WSS isn't used since Identifier already ends with whitespace
  def EnvVariableRule: Rule0 = rule:
    "env." ~ Identifier

  // put "openai" after the longer names to avoid partial matches
  // if an opening double quote is used, a closing one is required
  def ProviderRule: Rule0 = rule:
    (
      "openai-responses" | "openai-generic" | "openai" |
      "azure-openai" | "anthropic" | "google-ai" |
      "vertex-ai" | "aws-bedrock"
    ) ~ !(CharPredicate.AlphaNum | '_' | '-') ~ WSS |
    (
      '"' ~
      (
        "openai-responses" | "openai-generic" | "openai" |
        "azure-openai" | "anthropic" | "google-ai" |
        "vertex-ai" | "aws-bedrock"
      ) ~
      '"'
    ) ~ WSS

end BAMLTokens
