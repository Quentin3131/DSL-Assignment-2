package ch.usi.si.maise.edsl.baml.externalDSL

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.BeMatcher
import scala.util.Try

import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.wordspec.AnyWordSpec

// ignore this for now.
object successful extends BeMatcher[Try[?]]:
  override def apply(left: Try[?]): MatchResult = MatchResult(
    left.isSuccess,
    s"did not succeed (exception message: `${(left.recover: exception =>
      exception
    ).get}`)",
    "did succeed"
  )

trait TestUtils extends AnyWordSpec with should.Matchers with Inside:

  def checkResultAndParserCursor[T](
      stringToParse: String,
      result: Try[T],
      parser: BAMLParser,
      testSuccessful: Boolean
  ) =
    if (testSuccessful) then
      result shouldBe successful
      withClue(
        "Parser did not match the full length of the test string - Position "
      ):
        parser.cursor shouldBe (stringToParse.size)
    else if (parser.cursor == stringToParse.size) then
      result should not be successful

  def checkResultAndCursorWithActions[T](
      stringToParse: String,
      result: Try[T],
      parser: BAMLParserWithActions,
      testSuccessful: Boolean
  ) =
    if (testSuccessful) then
      result shouldBe successful
      withClue(
        "Parser did not match the full length of the test string - Position "
      ):
        parser.cursor shouldBe (stringToParse.size)
    else if (parser.cursor == stringToParse.size) then
      result should not be successful
