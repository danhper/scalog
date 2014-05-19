package com.tuvistavie.scalog.models

sealed trait Data

case class Clause(head: Atom, body: Formula) extends Data

case class Formula(atoms: List[Atom]) extends Data {
  def this(atoms: Atom*) = this(atoms.toList)
}
