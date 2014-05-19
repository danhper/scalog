package com.tuvistavie.scalog.ast

class Predicate(val identifier: String) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Predicate]

  override def equals(other: Any): Boolean = other match {
    case that: Predicate =>
      (that canEqual this) &&
        identifier == that.identifier
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(identifier)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Predicate {
  def apply(id: String) = new Predicate(id)
}
