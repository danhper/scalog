package com.tuvistavie.scalog
package parsers

import ast._

import org.specs2.mutable._

object ConcreteDatalogParser extends DatalogParser

class DatalogParserSpec extends Specification {
  lazy val parser = ConcreteDatalogParser

  def checkedParsed[T](parsed: parser.ParseResult[T], expected: Any) = {
    parsed.successful must beTrue
    val result = parsed.get
    result must_== expected
  }

  "The parser" should {
    "parse variables" in {
      val parsed = parser.parseAll(parser.variable, "Foo")
      checkedParsed(parsed, Variable("Foo"))
      parser.parseAll(parser.variable, "foo").successful must beFalse
    }

    "parse constants" in {
      val parsed = parser.parseAll(parser.constant, "foo")
      checkedParsed(parsed, Constant("foo"))
      parser.parseAll(parser.constant, "Foo").successful must beFalse
    }

    "parse symbols" in {
      val parsedConstant = parser.parseAll(parser.symbol, "foo")
      checkedParsed(parsedConstant, Constant("foo"))
      val parsedVariable = parser.parseAll(parser.symbol, "Foo")
      checkedParsed(parsedVariable, Variable("Foo"))
      parser.parseAll(parser.variable, "Foo-").successful must beFalse
    }

    "parse predicates" in {
      val parsed = parser.parseAll(parser.predicate, "foo")
      checkedParsed(parsed, Predicate("foo"))
      parser.parseAll(parser.predicate, "Foo").successful must beFalse
    }
  }

}
