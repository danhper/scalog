package com.tuvistavie.scalog
package parsers

import models._
import DatalogImplicits._

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
      checkedParsed(parsed, "Foo".v)
      parser.parseAll(parser.variable, "foo").successful must beFalse
    }

    "parse constants" in {
      val parsed = parser.parseAll(parser.constant, "foo")
      checkedParsed(parsed, "foo".c)
      parser.parseAll(parser.constant, "Foo").successful must beFalse
    }

    "parse symbols" in {
      val parsedConstant = parser.parseAll(parser.symbol, "foo")
      checkedParsed(parsedConstant, "foo".c)
      val parsedVariable = parser.parseAll(parser.symbol, "Foo")
      checkedParsed(parsedVariable, "Foo".v)
      parser.parseAll(parser.variable, "Foo-").successful must beFalse
    }

    "parse predicates" in {
      val parsed = parser.parseAll(parser.predicate, "foo")
      checkedParsed(parsed, "foo".p)
      parser.parseAll(parser.predicate, "Foo").successful must beFalse
    }

    "parse parameters" in {
      val parsed = parser.parseAll(parser.parameters, "a,B")
      checkedParsed(parsed, List("a".c, "B".v))
    }

    "parse parameters list" in {
      val parsed = parser.parseAll(parser.parametersList, "(a,B)")
      checkedParsed(parsed, List("a".c, "B".v))
    }

    "parse atom" in {
      "parse atom without parameters" in {
        val parsed = parser.parseAll(parser.atom, "foo")
        checkedParsed(parsed, "foo".a)
      }

      "parse atom with parameters" in {
        val parsed = parser.parseAll(parser.atom, "foo(X, y)")
        checkedParsed(parsed, "foo".a("X", "y"))
      }
    }

    "parse formula" in {
      "parse simple formula" in {
        val parsed = parser.parseAll(parser.formula, "foo(X)")
        checkedParsed(parsed, Formula("foo".a("X")))
      }

      "parse complex formula" in {
        val parsed = parser.parseAll(parser.formula, "foo(X, y), bar, baz(x, Y)")
        checkedParsed(parsed, "foo".a("X", "y") ~~ "bar" ~~ "baz".a("x", "Y"))
      }
    }

    "parse clause" in {
      val parsed = parser.parseAll(parser.clause, "foo(X) :- bar(X, baz)")
      checkedParsed(parsed, "foo".a("X") :- "bar".a("X", "baz"))
    }
  }
}
