package com.tuvistavie.scalog.models

import org.specs2.mutable.Specification


class AtomSpec extends Specification {
  "Atom" should {
    import DatalogImplicits._

    "be implicitly built" in {
      "foo".a must_== Atom("foo")
    }

    "generate clause" in {
      "foo" :- Formula("bar".a) must_== Clause("foo".a, Formula("bar".a))
    }

    "build with string arguments" in {
      "foo".a("x", "Y") must_== Atom("foo", List(Constant("x"), Variable("Y")))
    }

    "pretty format" in {
      "format without arguments" in {
        "foo".a.toString must_== "foo"
      }
      "format arguments" in {
        "foo".a("x", "Y").toString must_== "foo(x, Y)"
      }
    }
  }
}
