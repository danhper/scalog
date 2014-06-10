package com.tuvistavie.scalog.models

sealed trait Rule {
  def head: Atom
  def ruleName: String = head.name
  def arity: Int = head.arity
}

case class Fact(head: Atom) extends Rule {
  override def toString: String = head.toString
}

case class Clause(head: Atom, body: Formula) extends Rule {
  override def toString: String = head + " :- " + body
}
