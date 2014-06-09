package com.tuvistavie.scalog.models

class Formula(val atoms: List[Atom]) {
  def ~~(atom: Atom): Formula = Formula(atoms :+ atom: _*)

  def f: Formula = this

  override def toString: String = atoms mkString ", "

  def canEqual(other: Any): Boolean = other.isInstanceOf[Formula]

  override def equals(other: Any): Boolean = other match {
    case that: Formula =>
      (that canEqual this) &&
        atoms == that.atoms
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(atoms)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def q: Query = Query(this)

  def substitute(source: Symbol, target: Symbol): Formula = {
    Formula(atoms map { a => a.substitute(source, target) })
  }
}

object Formula {
  def apply(atoms: Atom*): Formula = new Formula(atoms.toList)
  def apply(atoms: List[Atom]): Formula = new Formula(atoms)
}
