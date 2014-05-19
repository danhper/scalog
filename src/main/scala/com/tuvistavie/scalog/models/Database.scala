package com.tuvistavie.scalog.models

class Database(_data: List[Data]) {
  val data: Map[String, Data] = _data map { d => (d.ruleName, d) } toMap

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

  override def toString: String = data mkString "\n"
}

object Database {
  def apply(data: List[Data]) = new Database(data)
}
