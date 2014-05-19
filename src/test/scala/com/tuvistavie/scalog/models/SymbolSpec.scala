package com.tuvistavie.scalog.models

import org.specs2.mutable.Specification

class SymbolSpec extends Specification {
  "Symbol" should {
    "be built from String" in {
      "build constant" in {
        Symbol("a") must_== Constant("a")
      }

      "build variables" in {
        Symbol("A") must_== Variable("A")
      }

      "throw on bad input" in {
        Symbol("a-_") must throwA[IllegalArgumentException]
      }
    }
  }
}
