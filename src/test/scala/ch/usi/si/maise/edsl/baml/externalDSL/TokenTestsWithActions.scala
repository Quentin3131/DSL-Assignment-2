package ch.usi.si.maise.edsl.baml.externalDSL

import org.scalatest.*
import flatspec.*
import matchers.*
import scala.util.Success

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Try
import ch.usi.si.maise.edsl.baml.model.*

/** This suites tests the parser combinators for tokens in the BAML language,
  * with semantic actions. There is no need to understand these code of these
  * tests. The test descriptions obtained by running the tests should be more
  * than enough. If you need any help, please contact the instructor. Do not
  * change this code.
  */
class TokenTestsWithActions
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

  def keywordTest(keyword: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(keyword)
    val result = parser.Keyword.run()
    checkResultAndCursorWithActions(keyword, result, parser, testSuccessful)

  def symbolTest(symbol: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(symbol)
    val result = parser.Symbol.run()
    checkResultAndCursorWithActions(symbol, result, parser, testSuccessful)

  def identifierTest(identifier: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(identifier)
    val result = parser.Identifier.run()
    checkResultAndCursorWithActions(identifier, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(Name(nameRep)) =>
          nameRep should equal(identifier.strip())
        case _ => fail("Identifier did not push a Name in the value stack.")
    end if

  def providerTest(provider: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(provider)
    val result = parser.ProviderRule.run()
    checkResultAndCursorWithActions(provider, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(p: BAMLProvider) =>
          p.rep should equal(provider.strip().stripPrefix("\"").stripSuffix("\""))
        case _ =>
          fail("Provider did not push a BAMLProvider in the value stack.")
    end if

  def stringLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(literal)
    val result = parser.StringLiteralRule.run()
    checkResultAndCursorWithActions(literal, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLStringValue(value)) =>
          value should equal(
            literal.strip().substring(1, literal.strip().size - 1)
          )
        case _ =>
          fail("String literal did not push a BAMLStringValue in the value stack.")
    end if

  def intLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(literal)
    val result = parser.IntLiteralRule.run()
    checkResultAndCursorWithActions(literal, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLIntValue(value)) =>
          value.toString should equal(literal.strip())
        case _ =>
          fail("Int literal did not push a BAMLIntValue in the value stack.")
    end if

  def floatLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(literal)
    val result = parser.FloatLiteralRule.run()
    checkResultAndCursorWithActions(literal, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLFloatValue(value)) =>
          value should equal(literal.strip().toDouble)
        case _ =>
          fail("Float literal did not push a BAMLFloatValue in the value stack.")
    end if

  def boolLiteralTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(literal)
    val result = parser.BoolLiteralRule.run()
    checkResultAndCursorWithActions(literal, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLBoolValue(value)) =>
          value.toString should equal(literal.strip())
        case _ =>
          fail("Bool literal did not push a BAMLBoolValue in the value stack.")
    end if

  def envVariableTest(envVariable: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(envVariable)
    val result = parser.EnvVariableRule.run()
    checkResultAndCursorWithActions(envVariable, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLEnvVariable(Name(nameRep))) =>
          nameRep should equal(envVariable.strip().stripPrefix("env."))
        case _ =>
          fail("Environment variable did not push a BAMLEnvVariable in the value stack.")
    end if

  def blockStringTest(literal: String, testSuccessful: Boolean = true) =
    val parser = BAMLParserWithActions(literal)
    val result = parser.BlockStringRule.run()
    checkResultAndCursorWithActions(literal, result, parser, testSuccessful)
    if (testSuccessful) then
      inside(result):
        case Success(BAMLPrompt(value)) =>
          value should equal(
            literal.strip().stripPrefix("#\"").stripSuffix("\"#")
          )
        case _ =>
          fail("Block string did not push a BAMLPrompt in the value stack.")
    end if

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
  val possibleBoolLiterals = List("true", "false")
  val invalidEnvVariables = Table("env.", "ENV.X", "env.1X", "env.class")
  val possibleBlockStrings = List(
    "#\"\"#",
    "#\"Extract the resume from: {{ resume }}\"#",
    "#\"\n  Say \"hello\" to {{ name }}.\n  {{ ctx.output_format }}\n\"#"
  )
  val invalidBlockStrings =
    Table("#\"unterminated", "\"plain\"", "# \"spaced\"#", "#\"wrong end\" #")

  "The BAML parser with semantic actions" when:
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
        s"recognize `${identifier}` as a possible identifier and correctly push a Name(\"${identifier}\") in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            identifierTest(s"${identifier}${spaceType}")
      invalidIdentifiers.foreach: (identifier) =>
        s"not recognize `${identifier}` as an valid identifier" in:
          identifierTest(s"$identifier", testSuccessful = false)

    "parsing providers" should:
      providers.foreach: (provider) =>
        s"recognize `${provider}`, bare or quoted, and push the corresponding BAMLProvider in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            providerTest(s"${provider}${spaceType}")
            providerTest(s"\"${provider}\"${spaceType}")
      "not recognize invalid providers" in:
        forEvery(invalidProviders): provider =>
          providerTest(provider, testSuccessful = false)

    "parsing string literals" should:
      possibleStrings.foreach: (stringLiteral) =>
        s"recognize ${stringLiteral} as a string literal and push a correct BAMLStringValue in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            stringLiteralTest(s"${stringLiteral}${spaceType}")
      "not recognize a string literal without an ending quoting mark" in:
        stringLiteralTest("\" an invalid string", testSuccessful = false)
      "not recognize a string literal spanning multiple lines" in:
        stringLiteralTest("\"an invalid\nstring\"", testSuccessful = false)

    "parsing int literals" should:
      possibleIntLiterals.foreach: (intLiteral) =>
        s"recognize ${intLiteral} as an int literal and push a correct BAMLIntValue in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            intLiteralTest(s"${intLiteral}${spaceType}")
      "not recognize invalid int literals" in:
        forEvery(invalidIntLiterals): intLiteral =>
          intLiteralTest(intLiteral, testSuccessful = false)

    "parsing float literals" should:
      possibleFloatLiterals.foreach: (floatLiteral) =>
        s"recognize ${floatLiteral} as a float literal and push a correct BAMLFloatValue in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            floatLiteralTest(s"${floatLiteral}${spaceType}")
      "not recognize invalid float literals" in:
        forEvery(invalidFloatLiterals): floatLiteral =>
          floatLiteralTest(floatLiteral, testSuccessful = false)

    "parsing bool literals" should:
      possibleBoolLiterals.foreach: (boolLiteral) =>
        s"recognize ${boolLiteral} as a bool literal and push a correct BAMLBoolValue in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            boolLiteralTest(s"${boolLiteral}${spaceType}")

    "parsing environment variables" should:
      "recognize `env.OPENAI_API_KEY` as an environment variable and push a correct BAMLEnvVariable in the value stack" in:
        forEvery(possibleSpaces): spaceType =>
          envVariableTest(s"env.OPENAI_API_KEY${spaceType}")
      "not recognize invalid environment variables" in:
        forEvery(invalidEnvVariables): envVariable =>
          envVariableTest(envVariable, testSuccessful = false)

    "parsing block strings" should:
      possibleBlockStrings.foreach: (blockString) =>
        s"recognize `${blockString.replace("\n", "\\n")}` as a block string and push a correct BAMLPrompt in the value stack" in:
          forEvery(possibleSpaces): spaceType =>
            blockStringTest(s"${blockString}${spaceType}")
      "not recognize invalid block strings" in:
        forEvery(invalidBlockStrings): blockString =>
          blockStringTest(blockString, testSuccessful = false)

  /* End of tests for basic requirements */

end TokenTestsWithActions

/** Add further tests in this class only.
  */
class AdditionalTokenTestsWithActions
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

end AdditionalTokenTestsWithActions
