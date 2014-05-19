package com.tuvistavie.scalog.ast


class Atom(val predicate: Predicate, val arguments: List[Symbol]) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Atom]

  override def equals(other: Any): Boolean = other match {
    case that: Atom =>
      (that canEqual this) &&
        predicate == that.predicate &&
        arguments == that.arguments
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(predicate, arguments)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Atom {
  def apply(pred: Predicate, args: List[Symbol]) = new Atom(pred, args)
}

