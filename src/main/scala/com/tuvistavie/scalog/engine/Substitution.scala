package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models.Symbol

class Substitution(val source: Symbol, val target: Symbol) {
  override def toString: String = source + " = " + target

  def canEqual(other: Any): Boolean = other.isInstanceOf[Substitution]

  override def equals(other: Any): Boolean = other match {
    case that: Substitution =>
      (that canEqual this) &&
        source == that.source &&
        target == that.target
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(source, target)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

