package com.tuvistavie.scalog.models

sealed trait Data

case class Clause(head: Atom, body: Formula) extends Data

case class Formula(atoms: Atom*) extends Data {
  def ~~(atom: Atom): Formula = Formula(atoms :+ atom: _*)
}

object Formula {
  def apply(atoms: List[Atom]): Formula = Formula(atoms: _*)
}

