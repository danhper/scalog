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
  }

}
