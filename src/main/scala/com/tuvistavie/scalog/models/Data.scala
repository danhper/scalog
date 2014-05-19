package com.tuvistavie.scalog.models

sealed trait Data

case class Clause(head: Atom, body: Formula) extends Data {
  override def toString: String = head + ":-" + body
}

case class Formula(atoms: Atom*) extends Data {
  def ~~(atom: Atom): Formula = Formula(atoms :+ atom: _*)

  def f: Formula = this

  override def toString: String = atoms mkString ", "
}

object Formula {
  def apply(atoms: List[Atom]): Formula = Formula(atoms: _*)
}

