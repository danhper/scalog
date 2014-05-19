package com.tuvistavie.scalog.models

sealed trait Data

case class Fact(head: Atom) extends Data {
  override def toString: String = head.toString
}

case class Clause(head: Atom, body: Formula) extends Data {
  override def toString: String = head + ":-" + body
}
