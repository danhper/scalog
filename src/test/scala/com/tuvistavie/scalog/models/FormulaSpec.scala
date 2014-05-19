package com.tuvistavie.scalog.models

import org.specs2.mutable.Specification

class FormulaSpec extends Specification {
  
  "Formula" should {
    import DatalogImplicits._
    
    "be implicitly built" in {
      Atom("foo").f must_== Formula("foo")
    }

    "be built from atoms" in {
      "foo".a ~~ "bar" must_== Formula("foo".a, "bar".a)
    }
  }

}
