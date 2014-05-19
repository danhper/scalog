package com.tuvistavie.scalog.ast

class Query(val formula: Formula) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Query]

  override def equals(other: Any): Boolean = other match {
    case that: Query =>
      (that canEqual this) &&
        formula == that.formula
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(formula)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Query {
  def apply(formula: Formula) = new Query(formula)
}

