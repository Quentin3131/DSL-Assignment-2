
package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*
import ch.usi.si.maise.edsl.baml.model.*

/** A parser for tokens in the BAML DSL, with semantic actions.
  */
trait BAMLTokensWithActions extends BAMLBase:

  /* Components of Identifiers and Literals */

  def UpperCaseLetter: Rule0 = rule:
    "A" - "Z"

  def LowerCaseLetter: Rule0 = rule:
    "a" - "z"

  def Letter: Rule0 = rule:
    LowerCaseLetter | UpperCaseLetter

  def Digit: Rule0 = rule:
    "0" - "9"

  def Underscore: Rule0 = rule:
    "_"

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

  // Keywords must not be followed by letters, digits or underscores.
  def keyword(rep: String) = rule:
    rep ~ !(Letter | Digit | Underscore) ~ WSS

  // Keywords
  def `class` = keyword("class")
  def `enum` = keyword("enum")
  def `client` = keyword("client")
  def `function` = keyword("function")
  def `llm` = keyword("llm")
  def `provider` = keyword("provider")
  def `options` = keyword("options")
  def `env` = keyword("env")
  def `prompt` = keyword("prompt")
  def `bool` = keyword("bool")
  def `int` = keyword("int")
  def `float` = keyword("float")
  def `string` = keyword("string")
  def `null` = keyword("null")
  def `true` = keyword("true")
  def `false` = keyword("false")
  def `@alias` = keyword("@alias")
  def `@description` = keyword("@description")

  // Symbols can be optionally followed by whitespace.
  def symbol(rep: String) = rule:
    rep ~ WSS

  // Symbols
  def `->` = symbol("->")
  def `<` = symbol("<")
  def `>` = symbol(">")
  def `{` = symbol("{")
  def `}` = symbol("}")
  def `(` = symbol("(")
  def `)` = symbol(")")
  def `[]` = symbol("[]")
  def `?` = symbol("?")
  def `:` = symbol(":")
  def `,` = symbol(",")
  def `.` = symbol(".")
  def `@` = symbol("@")

  // Match any possible keyword for testing.
  def Keyword: Rule0 = rule:
    `class` | `enum` | `int` | `string` | `client` |
    `function` | `bool` | `float` | `provider` | `options` |
    `env` | `prompt` | `llm` | `null` | `true` | `false` |
    `@alias` | `@description`

  // Match any possible symbol for testing.
  // Use string literals to avoid parboiled2 macro issues with symbolic methods.
  def Symbol: Rule0 = rule:
    (
      "->" | "<" | ">" | "{" | "}" |
      "(" | ")" | "[]" | "?" | ":" |
      "," | "." | "@"
    ) ~ WSS

  /* Identifiers and Literals with Semantic Actions */

  // Is not a keyword.
  // Starts with a letter or underscore.
  // Followed by zero or more letters, digits or underscores.
  def Identifier: Rule1[Name] = rule:
    !Keyword ~
    capture(
      (Letter | Underscore) ~
      zeroOrMore(Letter | Digit | Underscore)
    ) ~
    WSS ~>
    ((s: String) => Name(s))

  // Opening double quote, followed by zero or more characters
  // except double quotes, \n or \r, followed by a closing double quote.
  def StringLiteralRule: Rule1[BAMLStringValue] = rule:
    '"' ~
    capture(
      zeroOrMore(noneOf("\"\n\r"))
    ) ~
    '"' ~
    WSS ~>
    ((s: String) => BAMLStringValue(s))

  // Starts with '-' or no symbol.
  // If the first digit is 0, no additional digits can follow.
  // Otherwise, zero or more digits can follow.
  def IntLiteralRule: Rule1[BAMLIntValue] = rule:
    capture(
      optional('-') ~
      (
        '0' |
        (anyOf("123456789") ~ zeroOrMore(Digit))
      )
    ) ~
    WSS ~>
    ((s: String) => BAMLIntValue(s.toInt))

  // Starts with '-' or no symbol.
  // The integer part cannot have leading zeros.
  // A decimal point and at least one fractional digit are required.
  def FloatLiteralRule: Rule1[BAMLFloatValue] = rule:
    capture(
      optional('-') ~
      (
        '0' |
        (anyOf("123456789") ~ zeroOrMore(Digit))
      ) ~
      '.' ~
      oneOrMore(Digit)
    ) ~
    WSS ~>
    ((s: String) => BAMLFloatValue(s.toDouble))

  // True or false followed by optional whitespace.
  // The keyword helper already checks identifier boundaries.
  def BoolLiteralRule: Rule1[BAMLBoolValue] = rule:
    (`true` ~ push(BAMLBoolValue(true))) |
    (`false` ~ push(BAMLBoolValue(false)))

  // Starts with #"
  // Followed by zero or more characters until "# is reached.
  // Ends with "# and optional whitespace.
  def BlockStringRule: Rule1[BAMLPrompt] = rule:
    "#\"" ~
    capture(
      zeroOrMore(!"\"#" ~ ANY)
    ) ~
    "\"#" ~
    WSS ~>
    ((s: String) => BAMLPrompt(s))

  // "env." followed immediately by an identifier.
  // WSS isn't needed since Identifier already consumes whitespace.
  def EnvVariableRule: Rule1[BAMLEnvVariable] = rule:
    `env` ~
    `.` ~
    Identifier ~>
    ((name: Name) => BAMLEnvVariable(name))

  // Match supported providers, bare or quoted.
  // Longer names must appear before their prefixes.
  def ProviderRule: Rule1[BAMLProvider] = rule:
    (
      capture(
        "openai-responses" | "openai-generic" | "openai" |
        "azure-openai" | "anthropic" | "google-ai" |
        "vertex-ai" | "aws-bedrock"
      ) ~
      !(Letter | Digit | Underscore | '-') ~
      WSS |
      (
        '"' ~
        capture(
          "openai-responses" | "openai-generic" | "openai" |
          "azure-openai" | "anthropic" | "google-ai" |
          "vertex-ai" | "aws-bedrock"
        ) ~
        '"'
      )
    ) ~
    WSS ~>
    (
      (s: String) =>
      BAMLProvider.values.find(provider => provider.rep == s).get
    )

end BAMLTokensWithActions
