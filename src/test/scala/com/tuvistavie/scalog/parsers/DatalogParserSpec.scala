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

    "parse parameters" in {
      val parsed = parser.parseAll(parser.parameters, "a,B")
      checkedParsed(parsed, List(Constant("a"), Variable("B")))
    }

    "parse parameters list" in {
      val parsed = parser.parseAll(parser.parametersList, "(a,B)")
      checkedParsed(parsed, List(Constant("a"), Variable("B")))
    }

    "parse atom" in {
      "parse atom without parameters" in {
        val parsed = parser.parseAll(parser.atom, "foo")
        checkedParsed(parsed, Atom(Predicate("foo"), List.empty))
      }

      "parse atom with parameters" in {
        val parsed = parser.parseAll(parser.atom, "foo(X, y)")
        checkedParsed(parsed, Atom(Predicate("foo"), List(Variable("X"), Constant("y"))))
      }
    }

    "parse formula" in {
      "parse simple formula" in {
        val parsed = parser.parseAll(parser.formula, "foo(X)")
        checkedParsed(parsed, Formula(List(Atom(Predicate("foo"), List(Variable("X"))))))
      }

      "parse complex formula" in {
        val parsed = parser.parseAll(parser.formula, "foo(X, y), bar, baz(x, Y)")
        checkedParsed(parsed, Formula(List(
          Atom(Predicate("foo"), List(Variable("X"), Constant("y"))),
          Atom(Predicate("bar"), List.empty),
          Atom(Predicate("baz"), List(Constant("x"), Variable("Y")))
        )))
      }
    }
  }

}
