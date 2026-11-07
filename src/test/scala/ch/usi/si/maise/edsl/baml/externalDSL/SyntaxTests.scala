package ch.usi.si.maise.edsl.baml.externalDSL

import org.scalatest.*
import flatspec.*
import matchers.*

import org.scalatest.wordspec.AnyWordSpec
import scala.util.Try

/** This suites tests the parser combinators for the syntax of the BAML
  * language. There is no need to understand these code of these tests. The
  * test descriptions obtained by running the tests should be more than enough.
  * If you need any help, please contact the instructor. Do not change this
  * code.
  */
class SyntaxTests extends AnyWordSpec with should.Matchers with TestUtils:

  val types = List(
    "bool",
    "string",
    "null",
    "Resume",
    "string[]",
    "int?",
    "int?[]",
    "int[][]",
    "Resume?[][]",
    "Seniority  ?"
  )

  val invalidTypes = List(
    "int[",
    "[]int",
    "[]",
    "?",
    "?int"
  )

  def typeTest(typeDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(typeDecl)
    val result = parser.Type.run()
    checkResultAndParserCursor(typeDecl, result, parser, testSuccessful)

  val classFields = List(
    "name string",
    "age int?",
    "skills string[]",
    "scores float[][]",
    "level Seniority @alias(\"lvl\")",
    "bio string? @description(\"A short bio, max 200 chars.\")",
    "nick string @alias(\"nickname\") @description(\"how friends call them\")",
    "items Item?[]"
  )

  val invalidClassFields = List(
    "name",
    "name : string",
    "string name",
    "age int @alias(age)",
    "x int?[",
    "x int[ ]"
  )

  def classFieldTest(fieldDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(fieldDecl)
    val result = parser.ClassField.run()
    checkResultAndParserCursor(fieldDecl, result, parser, testSuccessful)

  val enumValues = List(
    "Junior",
    "Senior @alias(\"experienced\")",
    "Lead @description(\"leads a team\")",
    "Staff @alias(\"staff\") @description(\"above senior\")"
  )

  val invalidEnumValues = List(
    "123",
    "Junior = 1",
    "@alias(\"x\")",
    "enum"
  )

  def enumValueTest(valueDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(valueDecl)
    val result = parser.EnumValue.run()
    checkResultAndParserCursor(valueDecl, result, parser, testSuccessful)

  val clientOptions = List(
    "model \"gpt-4o\"",
    "api_key env.OPENAI_API_KEY",
    "temperature 0.7",
    "max_tokens 1024",
    "stream true",
    "base_url \"http://localhost:11434/v1\""
  )

  val invalidClientOptions = List(
    "model",
    "model: \"gpt-4o\"",
    "temperature .7",
    "api_key env",
    "\"model\" \"gpt-4o\""
  )

  def clientOptionTest(optionDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(optionDecl)
    val result = parser.ClientOption.run()
    checkResultAndParserCursor(optionDecl, result, parser, testSuccessful)

  val classes = List(
    "class Empty {}",
    "class Empty {\n}"
  ) ++ classFields.map: classField =>
    s"class Composed { $classField }"
  ++ classFields.grouped(2).toList.map: fieldPair =>
    s"class ClassWith2Fields { ${fieldPair.mkString("\n")} }"
  ++ List(s"class ClassWithManyFields {\n  ${classFields.mkString("\n  ")}\n}")

  val invalidClasses = List(
    "class {}",
    "class Foo",
    "class Foo { name }",
    "class int { x int }",
    s"class ClassWithOption { ${clientOptions(0)} }"
  )

  def classDeclarationTest(classDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(classDecl)
    val result = parser.ClassDeclaration.run()
    checkResultAndParserCursor(classDecl, result, parser, testSuccessful)

  val enums = enumValues.map: enumValue =>
    s"enum Composed { $enumValue }"
  ++ List(s"enum EnumWithManyValues {\n  ${enumValues.mkString("\n  ")}\n}")

  val invalidEnums = List(
    "enum Empty {}",
    "enum {}",
    "enum Foo",
    s"enum EnumWithField { ${classFields(0)} }",
    "enum Foo { 1 2 }"
  )

  def enumDeclarationTest(enumDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(enumDecl)
    val result = parser.EnumDeclaration.run()
    checkResultAndParserCursor(enumDecl, result, parser, testSuccessful)

  val clients = List(
    "client<llm> C { provider openai options { model \"gpt-4o\" } }",
    "client < llm > C {\n  provider \"anthropic\"\n  options {\n    model \"claude\"\n  }\n}"
  ) ++ clientOptions.map: clientOption =>
    s"client<llm> Composed { provider anthropic options { $clientOption } }"
  ++ List(
    s"client<llm> ClientWithManyOptions {\n  provider openai-generic\n  options {\n    ${clientOptions.mkString("\n    ")}\n  }\n}"
  )

  val invalidClients = List(
    "client C { provider openai options { model \"gpt-4o\" } }",
    "client<llm> C { provider openai }",
    "client<llm> C { options { model \"gpt-4o\" } provider openai }",
    "client<llm> C { provider mistral options { model \"gpt-4o\" } }",
    "client<embedding> C { provider openai options { model \"gpt-4o\" } }"
  )

  def clientDeclarationTest(clientDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(clientDecl)
    val result = parser.ClientDeclaration.run()
    checkResultAndParserCursor(clientDecl, result, parser, testSuccessful)

  val functions = List(
    "function Extract(text: string) -> Resume { client MyModel prompt #\"Extract: {{ text }}\"# }",
    "function Classify(text: string, levels: Seniority[]) -> Seniority? {\n  client MyModel\n  prompt #\"\n    Classify \"{{ text }}\".\n    {{ ctx.output_format }}\n  \"#\n}",
    "function Summarize(texts: string[], limits: int?[]) -> string { client C prompt #\"\"# }",
    "function F ( x : int )->int{client C prompt#\"x\"#}",
    "function lowercase(x: int) -> int { client C prompt #\"x\"# }"
  )

  val invalidFunctions = List(
    "function F() -> int { client C prompt #\"x\"# }",
    "function F(x: int) { client C prompt #\"x\"# }",
    "function F(x: int) -> int { prompt #\"x\"# }",
    "function F(x: int) -> int { client C }",
    "function F(x: int) -> int { client C prompt \"x\" }",
    "function F(x int) -> int { client C prompt #\"x\"# }",
    "function F(x: int) -> int { client<llm> C prompt #\"x\"# }"
  )

  def functionDeclarationTest(functionDecl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(functionDecl)
    val result = parser.FunctionDeclaration.run()
    checkResultAndParserCursor(functionDecl, result, parser, testSuccessful)

  def topLevelDeclarationTest(decl: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(decl)
    val result = parser.TopLevelElementDeclaration.run()
    checkResultAndParserCursor(decl, result, parser, testSuccessful)

  val files = List(
    "",
    s"\n  ${enums.last}\n\n${classes.last}\n",
    (enums ++ classes ++ clients ++ functions).mkString("\n\n")
  )

  val invalidFiles = List(
    s"${classes(0)} garbage",
    s"${classes(0)}\nclass"
  )

  def fileTest(file: String, testSuccessful: Boolean = true) =
    val parser = BAMLParser(file)
    val result = parser.BAMLDSL.run()
    checkResultAndParserCursor(file, result, parser, testSuccessful)

  "The BAML parser with no semantic actions" when:
    "parsing types" should:
      types.foreach: (typeDecl) =>
        s"recognize the `${typeDecl}` type correctly" in:
          typeTest(typeDecl)
      invalidTypes.foreach: (typeDecl) =>
        s"not recognize the `${typeDecl}` invalid type" in:
          typeTest(typeDecl, testSuccessful = false)
    "parsing class fields" should:
      classFields.foreach: (classField) =>
        s"recognize the `${classField}` class field correctly" in:
          classFieldTest(classField)
      invalidClassFields.foreach: (classField) =>
        s"not recognize the `${classField}` invalid class field" in:
          classFieldTest(classField, testSuccessful = false)
    "parsing enum values" should:
      enumValues.foreach: (enumValue) =>
        s"recognize the `${enumValue}` enum value correctly" in:
          enumValueTest(enumValue)
      invalidEnumValues.foreach: (enumValue) =>
        s"not recognize the `${enumValue}` invalid enum value" in:
          enumValueTest(enumValue, testSuccessful = false)
    "parsing client options" should:
      clientOptions.foreach: (clientOption) =>
        s"recognize the `${clientOption}` client option correctly" in:
          clientOptionTest(clientOption)
      invalidClientOptions.foreach: (clientOption) =>
        s"not recognize the `${clientOption}` invalid client option" in:
          clientOptionTest(clientOption, testSuccessful = false)
    "parsing class declarations" should:
      classes.foreach: (classDecl) =>
        s"recognize the `${classDecl}` class declaration correctly" in:
          classDeclarationTest(classDecl)
      invalidClasses.foreach: (invalidClass) =>
        s"not recognize the `${invalidClass}` invalid class declaration" in:
          classDeclarationTest(invalidClass, testSuccessful = false)
    "parsing enum declarations" should:
      enums.foreach: (enumDecl) =>
        s"recognize the `${enumDecl}` enum declaration correctly" in:
          enumDeclarationTest(enumDecl)
      invalidEnums.foreach: (invalidEnum) =>
        s"not recognize the `${invalidEnum}` invalid enum declaration" in:
          enumDeclarationTest(invalidEnum, testSuccessful = false)
    "parsing client declarations" should:
      clients.foreach: (clientDecl) =>
        s"recognize the `${clientDecl}` client declaration correctly" in:
          clientDeclarationTest(clientDecl)
      invalidClients.foreach: (invalidClient) =>
        s"not recognize the `${invalidClient}` invalid client declaration" in:
          clientDeclarationTest(invalidClient, testSuccessful = false)
    "parsing function declarations" should:
      functions.foreach: (functionDecl) =>
        s"recognize the `${functionDecl.replace("\n", "\\n")}` function declaration correctly" in:
          functionDeclarationTest(functionDecl)
      invalidFunctions.foreach: (invalidFunction) =>
        s"not recognize the `${invalidFunction}` invalid function declaration" in:
          functionDeclarationTest(invalidFunction, testSuccessful = false)
    "parsing top level declarations" should:
      (classes ++ enums ++ clients ++ functions).foreach: (decl) =>
        s"recognize the `${decl}` as top level declaration correctly" in:
          topLevelDeclarationTest(decl)
    "parsing BAML files" should:
      files.zipWithIndex.foreach: (file, index) =>
        s"recognize the BAML file number ${index + 1} correctly" in:
          fileTest(file)
      invalidFiles.foreach: (file) =>
        s"not recognize the `${file}` invalid BAML file" in:
          fileTest(file, testSuccessful = false)
end SyntaxTests

/** Add further tests in this class only.
  */
class AdditionalSyntaxTests
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

end AdditionalSyntaxTests
