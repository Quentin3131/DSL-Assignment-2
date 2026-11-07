package ch.usi.si.maise.edsl.baml.externalDSL

import org.scalatest.*
import flatspec.*
import matchers.*

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Try

/** This suites tests the parser combinators for tokens in the BAML language.
  * There is no need to understand these code of these tests. The test
  * descriptions obtained by running the tests should be more than enough. If
  * you need any help, please contact the instructor. Do not change this code.
  */
class TokenTests extends AnyWordSpec with should.Matchers with TestUtils:

  def keywordTest(keyword: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(keyword)
    val result = parser.Keyword.run()
    checkResultAndParserCursor(keyword, result, parser, testSuccessful)

  def symbolTest(symbol: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(symbol)
    val result = parser.Symbol.run()
    checkResultAndParserCursor(symbol, result, parser, testSuccessful)

  def identifierTest(identifier: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(identifier)
    val result = parser.Identifier.run()
    checkResultAndParserCursor(identifier, result, parser, testSuccessful)

  def providerTest(provider: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(provider)
    val result = parser.ProviderRule.run()
    checkResultAndParserCursor(provider, result, parser, testSuccessful)

  def stringLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(literal)
    val result = parser.StringLiteralRule.run()
    checkResultAndParserCursor(literal, result, parser, testSuccessful)

  def intLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(literal)
    val result = parser.IntLiteralRule.run()
    checkResultAndParserCursor(literal, result, parser, testSuccessful)

  def floatLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(literal)
    val result = parser.FloatLiteralRule.run()
    checkResultAndParserCursor(literal, result, parser, testSuccessful)

  def envVariableTest(envVariable: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(envVariable)
    val result = parser.EnvVariableRule.run()
    checkResultAndParserCursor(envVariable, result, parser, testSuccessful)

  def blockStringTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(literal)
    val result = parser.BlockStringRule.run()
    checkResultAndParserCursor(literal, result, parser, testSuccessful)

  val possibleSpaces = Table("", "  ", "\n", " \n  ", "\t\n \t\r\n ")

  val keywords = List(
    "class", "enum", "client", "llm", "provider", "options", "env",
    "true", "false", "bool", "int", "float", "string", "null", "function",
    "prompt", "@alias", "@description"
  )
  val keywordPrefixes = Table("classes", "integer", "prompts", "nullable")
  val symbols =
    List("{", "}", "(", ")", "<", ">", ",", ".", ":", "->", "?", "[]")
  val possibleIdentifiers = List("foo", "Resume", "_test", "api_key")
  val invalidIdentifiers = List("1foo", "class", "foo-bar")

  val providers = List(
    "openai", "openai-responses", "openai-generic", "azure-openai",
    "anthropic", "google-ai", "vertex-ai", "aws-bedrock"
  )
  val invalidProviders =
    Table("openai2", "OpenAI", "open-ai", "\"openai", "mistral")

  val possibleStrings = List("\"\"", "\"gpt-4o, e.g. claude-sonnet-4.5!\"")
  val possibleIntLiterals = List("0", "-42", "999123")
  val invalidIntLiterals = Table("01", "A", "-", "_1")
  val possibleFloatLiterals = List("0.7", "-12.25")
  val invalidFloatLiterals = Table("1.", ".5", "01.2", "1,5")
  val invalidEnvVariables = Table("env.", "ENV.X", "env.1X", "env.class")
  val possibleBlockStrings = List(
    "#\"\"#",
    "#\"Extract the resume from: {{ resume }}\"#",
    "#\"\n  Say \"hello\" to {{ name }}.\n  {{ ctx.output_format }}\n\"#"
  )
  val invalidBlockStrings =
    Table("#\"unterminated", "\"plain\"", "# \"spaced\"#", "#\"wrong end\" #")

  "The BAML parser with no semantic actions" when:
    "parsing keywords" should:
      keywords.foreach: (keyword) =>
        s"recognize the `${keyword}` keyword with space variants" in:
          forEvery(possibleSpaces): spaceType =>
            keywordTest(s"${keyword}${spaceType}")
      "not recognize a keyword as the prefix of a longer word" in:
        forEvery(keywordPrefixes): word =>
          keywordTest(word, testSuccessful = false)

    "parsing symbols" should:
      symbols.foreach: (symbol) =>
        s"recognize the `${symbol}` symbol with space variants" in:
          forEvery(possibleSpaces): spaceType =>
            symbolTest(s"${symbol}${spaceType}")
      "not recognize `[ ]` as a valid Symbol" in:
        symbolTest("[ ]", testSuccessful = false)

    "parsing identifiers" should:
      possibleIdentifiers.foreach: (identifier) =>
        s"recognize `${identifier}` as a possible identifier" in:
          forEvery(possibleSpaces): spaceType =>
            identifierTest(s"${identifier}${spaceType}")
      invalidIdentifiers.foreach: (identifier) =>
        s"not recognize `${identifier}` as an valid identifier" in:
          identifierTest(s"$identifier", testSuccessful = false)

    "parsing providers" should:
      providers.foreach: (provider) =>
        s"recognize `${provider}` as a provider, bare or quoted" in:
          forEvery(possibleSpaces): spaceType =>
            providerTest(s"${provider}${spaceType}")
            providerTest(s"\"${provider}\"${spaceType}")
      "not recognize invalid providers" in:
        forEvery(invalidProviders): provider =>
          providerTest(provider, testSuccessful = false)

    "parsing string literals" should:
      possibleStrings.foreach: (stringLiteral) =>
        s"recognize ${stringLiteral} as a string literal" in:
          forEvery(possibleSpaces): spaceType =>
            stringLiteralTest(s"${stringLiteral}${spaceType}")
      "not recognize a string literal without an ending quoting mark" in:
        stringLiteralTest("\" an invalid string", testSuccessful = false)
      "not recognize a string literal spanning multiple lines" in:
        stringLiteralTest("\"an invalid\nstring\"", testSuccessful = false)

    "parsing int literals" should:
      possibleIntLiterals.foreach: (intLiteral) =>
        s"recognize ${intLiteral} as an int literal" in:
          forEvery(possibleSpaces): spaceType =>
            intLiteralTest(s"${intLiteral}${spaceType}")
      "not recognize invalid int literals" in:
        forEvery(invalidIntLiterals): intLiteral =>
          intLiteralTest(intLiteral, testSuccessful = false)

    "parsing float literals" should:
      possibleFloatLiterals.foreach: (floatLiteral) =>
        s"recognize ${floatLiteral} as a float literal" in:
          forEvery(possibleSpaces): spaceType =>
            floatLiteralTest(s"${floatLiteral}${spaceType}")
      "not recognize invalid float literals" in:
        forEvery(invalidFloatLiterals): floatLiteral =>
          floatLiteralTest(floatLiteral, testSuccessful = false)

    "parsing environment variables" should:
      "recognize `env.OPENAI_API_KEY` as an environment variable" in:
        forEvery(possibleSpaces): spaceType =>
          envVariableTest(s"env.OPENAI_API_KEY${spaceType}")
      "not recognize invalid environment variables" in:
        forEvery(invalidEnvVariables): envVariable =>
          envVariableTest(envVariable, testSuccessful = false)

    "parsing block strings" should:
      possibleBlockStrings.foreach: (blockString) =>
        s"recognize `${blockString.replace("\n", "\\n")}` as a block string" in:
          forEvery(possibleSpaces): spaceType =>
            blockStringTest(s"${blockString}${spaceType}")
      "not recognize invalid block strings" in:
        forEvery(invalidBlockStrings): blockString =>
          blockStringTest(blockString, testSuccessful = false)

  /* End of tests for basic requirements */

end TokenTests

/** Add further tests in this class only.
  */
class AdditionalTokenTests
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

end AdditionalTokenTests
