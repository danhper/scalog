package com.tuvistavie.scalog.models

import scala.collection.mutable

trait Database {
  def hasRule(name: String, arity: Int): Boolean
  def getRule(name: String, arity: Int): Option[List[Rule]]
  def addRule(data: Rule): Unit
  def addRules(data: List[Rule]): Unit
}

class MapDatabase(_rules: List[Rule]) extends Database {
  type Key = (String, Int)

  private var rules: Map[Key, List[Rule]] = rulesListToMap(_rules)

  def hasRule(name: String, arity: Int): Boolean = rules.contains((name, arity))
  def getRule(name: String, arity: Int): Option[List[Rule]] = rules.get((name, arity))

  def addRule(rule: Rule): Unit = {
    val key = (rule.ruleName, rule.arity)
    if (rules.contains(key)) rules += key -> (rule :: rules(key))
    else rules += key -> List(rule)
  }

  def addRules(rule: List[Rule]): Unit = {
    rules ++= rulesListToMap(rule)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Database]

  override def equals(other: Any): Boolean = other match {
    case that: MapDatabase =>
      (that canEqual this) &&
        rules == that.rules
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(rules)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = rules mkString "\n"

  private def rulesListToMap(rules: List[Rule]): Map[Key, List[Rule]] = {
    val map: mutable.Map[Key, List[Rule]] = mutable.Map.empty
    rules foreach { d =>
      val key = (d.ruleName, d.arity)
      if (map.contains(key)) map += key -> (d :: map(key))
      else map += key -> List(d)
    }
    map toMap
  }
}

object Database {
  def apply(data: List[Rule]): Database = new MapDatabase(data)
}
