package com.tuvistavie.scalog.models

class Predicate(val name: String) extends NamedEntity {
  checkName(name)

  val regexp = Predicate.regexp

  def canEqual(other: Any): Boolean = other.isInstanceOf[Predicate]

  override def equals(other: Any): Boolean = other match {
    case that: Predicate =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Predicate {
  def apply(name: String) = new Predicate(name)
  val regexp  = "[a-z][a-z0-9_]*"
}
