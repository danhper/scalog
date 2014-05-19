package com.tuvistavie.scalog

import com.tuvistavie.scalog.parsers.DatalogParser
import com.tuvistavie.scalog.models.{AstImplicits, Atom}

import AstImplicits._

object Main extends DatalogParser {
  def main(args: Array[String]): Unit = {
    val foo: Atom = "foo".a("a", "x")
  }
}
