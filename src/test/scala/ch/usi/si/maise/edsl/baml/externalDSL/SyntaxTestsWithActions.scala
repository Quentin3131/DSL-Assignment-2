package ch.usi.si.maise.edsl.baml.externalDSL

import org.scalatest.*
import flatspec.*
import matchers.*

import org.scalatest.wordspec.AnyWordSpec
import ch.usi.si.maise.edsl.baml.model.*
import ch.usi.si.maise.edsl.baml.model.BAMLPrimitiveType.*
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import org.parboiled2.ParseError
import org.parboiled2.RuleTrace
import org.parboiled2.RuleTrace.Fail

/** This suites tests the parser combinators for the syntax of the BAML
  * language, with semantic actions. There is no need to understand these code
  * of these tests. The test descriptions obtained by running the tests should
  * be more than enough. If you need any help, please contact the instructor.
  * Do not change this code.
  */
class SyntaxTestsWithActions
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

  val fieldsWithPrimitiveTypes = List(
    BAMLClassField(Name("name"), StringType),
    BAMLClassField(Name("age"), BAMLOptionalType(IntType)),
    BAMLClassField(Name("scores"), BAMLListType(FloatType)),
    BAMLClassField(Name("flags"), BAMLListType(BAMLOptionalType(BoolType))),
    BAMLClassField(Name("matrix"), BAMLListType(BAMLListType(IntType))),
    BAMLClassField(Name("tags"), BAMLListType(BAMLListType(StringType))),
    BAMLClassField(Name("nothing"), NullType, Some(BAMLStringValue("none"))),
    BAMLClassField(
      Name("bio"),
      BAMLOptionalType(StringType),
      Some(BAMLStringValue("biography")),
      Some(BAMLStringValue("A short bio, max 200 chars."))
    )
  )

  val classesWithPrimitiveTypes = List(
    BAMLClassDeclaration(Name("Empty"))
  ) ++ fieldsWithPrimitiveTypes.map: field =>
    BAMLClassDeclaration(Name("Composed"), Seq(field))
  ++ fieldsWithPrimitiveTypes.grouped(2).toList.map: fields =>
    BAMLClassDeclaration(Name("Composed"), fields)
  ++ List(BAMLClassDeclaration(Name("Composed"), fieldsWithPrimitiveTypes))

  def classDeclarationTest(classDecl: BAMLClassDeclaration) =
    val parser = BAMLParserWithActions(classDecl.rep)
    val result = parser.ClassDeclaration.run()
    checkResultAndCursorWithActions(classDecl.rep, result, parser, true)
    inside(result):
      case Success(parsedClass) =>
        parsedClass should equal(classDecl)
    parser.symbolTable.resolveClass(classDecl.name) should equal(
      Success(BAMLClassReference(classDecl.name))
    )

  val enumValues = List(
    BAMLEnumValue(Name("Junior")),
    BAMLEnumValue(Name("Senior"), Some(BAMLStringValue("experienced"))),
    BAMLEnumValue(Name("Lead"), None, Some(BAMLStringValue("leads a team"))),
    BAMLEnumValue(
      Name("Staff"),
      Some(BAMLStringValue("staff")),
      Some(BAMLStringValue("above senior"))
    )
  )

  val enums = enumValues.map: value =>
    BAMLEnumDeclaration(Name("Composed"), Seq(value))
  ++ List(BAMLEnumDeclaration(Name("Seniority"), enumValues))

  def enumDeclarationTest(enumDecl: BAMLEnumDeclaration) =
    val parser = BAMLParserWithActions(enumDecl.rep)
    val result = parser.EnumDeclaration.run()
    checkResultAndCursorWithActions(enumDecl.rep, result, parser, true)
    inside(result):
      case Success(parsedEnum) =>
        parsedEnum should equal(enumDecl)
    parser.symbolTable.resolveEnum(enumDecl.name) should equal(
      Success(BAMLEnumReference(enumDecl.name))
    )

  val modelOption = BAMLClientOption(Name("model"), BAMLStringValue("gpt-4o"))

  val otherClientOptions = List(
    BAMLClientOption(Name("api_key"), BAMLEnvVariable(Name("OPENAI_API_KEY"))),
    BAMLClientOption(Name("temperature"), BAMLFloatValue(0.7)),
    BAMLClientOption(Name("max_tokens"), BAMLIntValue(1024)),
    BAMLClientOption(Name("stream"), BAMLBoolValue(true)),
    BAMLClientOption(Name("base_url"), BAMLStringValue("http://localhost:11434/v1"))
  )

  val clients = BAMLProvider.values.toList.map: provider =>
    BAMLClientDeclaration(Name("Client"), provider, Seq(modelOption))
  ++ otherClientOptions.map: option =>
    BAMLClientDeclaration(Name("Client"), BAMLProvider.Anthropic, Seq(modelOption, option))
  ++ List(
    BAMLClientDeclaration(
      Name("Client"),
      BAMLProvider.OpenAIGeneric,
      otherClientOptions ++ Seq(modelOption)
    )
  )

  def clientDeclarationTest(clientDecl: BAMLClientDeclaration) =
    val parser = BAMLParserWithActions(clientDecl.rep)
    val result = parser.ClientDeclaration.run()
    checkResultAndCursorWithActions(clientDecl.rep, result, parser, true)
    inside(result):
      case Success(parsedClient) =>
        parsedClient should equal(clientDecl)
    parser.symbolTable.resolveType(clientDecl.name) should not be successful

  /** Declarations that are available to the classes referring to other types.
    */
  val referencedDeclarations =
    "enum Seniority { Junior Senior }\nclass Education { school string }\n" +
      "client<llm> MyModel { provider openai options { model \"gpt-4o\" } }"

  /** Fields that refer to declared types, paired with the first type name they
    * refer to.
    */
  val fieldsWithReferences = List(
    BAMLClassField(Name("level"), BAMLEnumReference(Name("Seniority"))) -> "Seniority",
    BAMLClassField(
      Name("education"),
      BAMLListType(BAMLClassReference(Name("Education")))
    ) -> "Education",
    BAMLClassField(
      Name("previous"),
      BAMLOptionalType(BAMLClassReference(Name("Education")))
    ) -> "Education",
    BAMLClassField(
      Name("levels"),
      BAMLListType(BAMLOptionalType(BAMLEnumReference(Name("Seniority"))))
    ) -> "Seniority"
  )

  def classWithReferencesTest(field: BAMLClassField) =
    val declarationParser = BAMLParserWithActions(referencedDeclarations)
    val declarationResult = declarationParser.BAMLDSL.run()
    declarationResult shouldBe successful
    val symbolTable = declarationParser.symbolTable
    val classDecl = BAMLClassDeclaration(Name("Resume"), Seq(field))
    val parser = BAMLParserWithActions(classDecl.rep, symbolTable)
    val result = parser.ClassDeclaration.run()
    result shouldBe successful
    inside(result):
      case Success(parsedClass) =>
        parsedClass should equal(classDecl)

  val clientReference = BAMLClientReference(Name("MyModel"))

  val functions = List(
    BAMLFunctionDeclaration(
      Name("Extract"),
      Seq(BAMLParameter(Name("text"), StringType)),
      BAMLClassReference(Name("Education")),
      clientReference,
      BAMLPrompt("Extract: {{ text }}")
    ),
    BAMLFunctionDeclaration(
      Name("Classify"),
      Seq(
        BAMLParameter(Name("text"), StringType),
        BAMLParameter(Name("hints"), BAMLListType(StringType))
      ),
      BAMLOptionalType(BAMLEnumReference(Name("Seniority"))),
      clientReference,
      BAMLPrompt("\n    Classify \"{{ text }}\".\n    {{ ctx.output_format }}\n  ")
    ),
    BAMLFunctionDeclaration(
      Name("Summarize"),
      Seq(
        BAMLParameter(Name("texts"), BAMLListType(StringType)),
        BAMLParameter(Name("limits"), BAMLListType(BAMLOptionalType(IntType)))
      ),
      StringType,
      clientReference,
      BAMLPrompt("")
    ),
    BAMLFunctionDeclaration(
      Name("ByLevel"),
      Seq(BAMLParameter(Name("level"), BAMLEnumReference(Name("Seniority")))),
      BAMLListType(BAMLListType(BAMLClassReference(Name("Education")))),
      clientReference,
      BAMLPrompt("Group by {{ level }}.")
    )
  )

  def functionDeclarationTest(functionDecl: BAMLFunctionDeclaration) =
    val declarationParser = BAMLParserWithActions(referencedDeclarations)
    val declarationResult = declarationParser.BAMLDSL.run()
    declarationResult shouldBe successful
    val symbolTable = declarationParser.symbolTable
    val parser = BAMLParserWithActions(functionDecl.rep, symbolTable)
    val result = parser.FunctionDeclaration.run()
    checkResultAndCursorWithActions(functionDecl.rep, result, parser, true)
    inside(result):
      case Success(parsedFunction) =>
        parsedFunction should equal(functionDecl)
    parser.symbolTable.resolveType(functionDecl.name) should not be successful
    parser.symbolTable.resolveClient(functionDecl.name) should not be successful

  /** Checks that parsing the whole BAML file fails with the expected semantic
    * error message.
    */
  def semanticFailureTest(file: String, expectedMessage: String) =
    val parser = BAMLParserWithActions(file)
    val result = parser.BAMLDSL.run()
    result should not be successful
    inside(result):
      case Failure(ParseError(_, _, RuleTrace(_, Fail(actualMessage)) :: _)) =>
        actualMessage.replaceAll("\\[|\\]", "") should startWith(expectedMessage)

  val constraintViolations = List(
    "class A { xs int[]? }" ->
      "requirement failed: list types cannot be optional",
    "class A { xs int?[]? }" ->
      "requirement failed: list types cannot be optional",
    "client<llm> C { provider openai options { temperature 0.5 } }" ->
      "requirement failed: a client must have a model option with a string value"
  )

  val duplicateDeclarations = List(
    "class A { x int }\nclass A { y int }",
    "enum A { X }\nclass A { x int }",
    "class A { x int }\nclient<llm> A { provider openai options { model \"gpt-4o\" } }"
  )

  val invalidReferences = List(
    "class Node { next Node? }" -> "Type named Node has not been declared",
    "class A { b B }\nclass B { x int }" -> "Type named B has not been declared",
    "client<llm> A { provider openai options { model \"gpt-4o\" } }\nclass B { c A }" ->
      "Name A has not been declared as a type"
  )

  val functionWithModel =
    "client<llm> MyModel { provider openai options { model \"gpt-4o\" } }\n"

  val invalidFunctions = List(
    "function F(x: int) -> int { client C prompt #\"x\"# }" ->
      "client named C has not been declared",
    "class C { x int }\nfunction F(x: int) -> int { client C prompt #\"x\"# }" ->
      "Name C has not been declared as a client",
    s"${functionWithModel}function f(x: int) -> int { client MyModel prompt #\"x\"# }" ->
      "requirement failed: function names must start with an uppercase letter",
    s"${functionWithModel}function F(x: int, x: string) -> int { client MyModel prompt #\"x\"# }" ->
      "requirement failed: parameter names must be distinct",
    s"${functionWithModel}function F(x: Resume) -> int { client MyModel prompt #\"x\"# }" ->
      "Type named Resume has not been declared",
    s"${functionWithModel}function F(x: int) -> int { client MyModel prompt #\"x\"# }\nclass A { f F }" ->
      "Name F has not been declared as a type",
    s"class F { x int }\n${functionWithModel}function F(x: int) -> int { client MyModel prompt #\"x\"# }" ->
      "name F has been already declared as"
  )

  val files = List(
    BAMLFile(Seq()),
    BAMLFile(
      Seq(
        BAMLEnumDeclaration(Name("Seniority"), enumValues),
        BAMLClassDeclaration(Name("Education"), fieldsWithPrimitiveTypes),
        BAMLClassDeclaration(Name("Resume"), fieldsWithReferences.map(_._1)),
        clients.last.copy(name = Name("MyModel"))
      ) ++ functions
    )
  )

  def fileTest(file: BAMLFile) =
    val parser = BAMLParserWithActions(file.rep)
    val result = parser.BAMLDSL.run()
    checkResultAndCursorWithActions(file.rep, result, parser, true)
    inside(result):
      case Success(parsedFile) =>
        parsedFile should equal(file)

  "The BAML parser with semantic actions" when:

    "parsing class declarations with primitive types" should:
      classesWithPrimitiveTypes.foreach: (classDecl) =>
        s"recognize the `${classDecl.testRep}` class declaration correctly, reconstruct semantic model object, and declare it in the symbol table" in:
          classDeclarationTest(classDecl)

    "parsing enum declarations" should:
      enums.foreach: (enumDecl) =>
        s"recognize the `${enumDecl.testRep}` enum declaration correctly, reconstruct semantic model object, and declare it in the symbol table" in:
          enumDeclarationTest(enumDecl)

    "parsing client declarations" should:
      clients.foreach: (clientDecl) =>
        s"recognize the `${clientDecl.testRep}` client declaration correctly, reconstruct semantic model object, and declare it in the symbol table" in:
          clientDeclarationTest(clientDecl)

    "parsing class declarations with references to declared types" should:
      fieldsWithReferences.foreach: (field, _) =>
        s"recognize a class with field `${field.rep}` when the referred types have been declared upfront, and reconstruct semantic model object" in:
          classWithReferencesTest(field)

    "parsing class declarations with references to undeclared types" should:
      fieldsWithReferences.foreach: (field, typeName) =>
        s"fail to recognize a class with field `${field.rep}` when the referred types have not been declared, with proper message" in:
          semanticFailureTest(
            BAMLClassDeclaration(Name("Resume"), Seq(field)).rep,
            s"Type named ${typeName} has not been declared"
          )

    "parsing declarations violating the constraints of the model" should:
      constraintViolations.foreach: (file, expectedMessage) =>
        s"fail to recognize `${file.replace("\n", "\\n")}` with proper message" in:
          semanticFailureTest(file, expectedMessage)

    "parsing duplicate declarations" should:
      duplicateDeclarations.foreach: (file) =>
        s"fail to recognize `${file.replace("\n", "\\n")}` with proper message" in:
          semanticFailureTest(file, "name A has been already declared as")

    "parsing references to the declared class itself, to types declared later, or to clients" should:
      invalidReferences.foreach: (file, expectedMessage) =>
        s"fail to recognize `${file.replace("\n", "\\n")}` with proper message" in:
          semanticFailureTest(file, expectedMessage)

    "parsing function declarations" should:
      functions.foreach: (functionDecl) =>
        s"recognize the `${functionDecl.testRep}` function declaration when its types and client have been declared upfront, reconstruct semantic model object, and declare it in the symbol table" in:
          functionDeclarationTest(functionDecl)

    "parsing invalid function declarations" should:
      invalidFunctions.foreach: (file, expectedMessage) =>
        s"fail to recognize `${file.replace("\n", "\\n")}` with proper message" in:
          semanticFailureTest(file, expectedMessage)

    "parsing BAML files" should:
      files.zipWithIndex.foreach: (file, index) =>
        s"recognize the BAML file number ${index + 1} correctly and reconstruct semantic model object" in:
          fileTest(file)

end SyntaxTestsWithActions

/** Add further tests in this class only.
  */
class AdditionalSyntaxTestsWithActions
    extends AnyWordSpec
    with should.Matchers
    with TestUtils:

end AdditionalSyntaxTestsWithActions
