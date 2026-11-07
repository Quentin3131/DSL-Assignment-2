package ch.usi.si.maise.edsl.baml.externalDSL

import org.parboiled2.*
import scala.util.Try

trait BAMLBase extends Parser:
  /** Ignore this for now, but it allows to automatically turn a Failure in a
    * Try into a parser failure, otherwise returning the value in Success.
    */
  extension [T](t: Try[T])
    inline def getOrFailParser = rule:
      (test(t.isSuccess) ~ push(t.get)) | failX(t.failed.get.getMessage())
