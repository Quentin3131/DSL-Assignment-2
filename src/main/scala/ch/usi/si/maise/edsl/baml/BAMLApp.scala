package ch.usi.si.maise.edsl.baml

import scala.util.{Success, Failure}
import ch.usi.si.maise.edsl.baml.externalDSL.*
import ch.usi.si.maise.edsl.baml.model.BAMLElement
import scala.io.Source
import java.nio.file.Path
import java.nio.file.Files
import scala.util.Try
import org.parboiled2.ParseError
import org.parboiled2.Parser
import org.parboiled2.ErrorFormatter
import org.parboiled2.RuleTrace.Fail


/**
  * Entrypoint for parser without actions.
  *
  * @param args path for the BAML file to analyze.
  */
@main def bamlParser(args: String*): Unit =
  runParserLogic(args.toList): dslScript =>
    val parser = BAMLParser(dslScript)
    (parser, parser.BAMLDSL.run())


@main def bamlParserWithActions(args: String*): Unit =
  runParserLogic(args.toList): dslScript =>
    val parser = BAMLParserWithActions(dslScript)
    (parser, parser.BAMLDSL.run())

/**
  * Ignore the purpose of this method.
  */
def runParserLogic[T](args: List[String])(logic: (dslScript: String) => (Parser, Try[T])) =
  args match
    case Nil => println(s"Please specify at least one file to parse.")
    case dslFile :: xs =>
      val path = Path.of(dslFile)
      if Files.exists(path) then
        val dslScript = Source.fromFile(dslFile).getLines().mkString("\n")
        val (parser, tryResult) = logic(dslScript)
        val result = tryResult match
          case Success(value) => value
          case Failure(parseError: ParseError) => getParseErrorRep(parser, parseError)
          case Failure(t) => s"Unexpected Exception: ${t}"

        println(s"******\nParser Result:\n${result}")
        result match
          case element: BAMLElement =>
            println(s"******\nResult Representation:\n${element.rep.stripIndent()}")
          case _ => ()

      else
        println(s"File ${dslFile} does not exist.")
      end if


def getParseErrorRep(parser: Parser, parseError: ParseError): String =
  parseError.traces.head.terminal match
    case Fail(expected) => s"Semantic Error (line ${parseError.position.line}, column: ${parseError.position.column}): ${expected}"
    case _ => parser.formatError(parseError, new ErrorFormatter(showExpected = true, showPosition = true, showLine = true, showTraces = true))
