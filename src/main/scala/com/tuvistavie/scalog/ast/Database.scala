package com.tuvistavie.scalog.ast

class Database(val data: List[Data]) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Database]

  override def equals(other: Any): Boolean = other match {
    case that: Database =>
      (that canEqual this) &&
        data == that.data
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(data)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Database {
  def apply(data: List[Data]) = new Database(data)
}
