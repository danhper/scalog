package com.tuvistavie.scalog.models

import scala.collection.mutable

trait Database extends mutable.Map[(String, Int), List[Rule]] {
  type Key = (String, Int)

  def +=(rule: Rule): this.type = addRule(rule)
  def +=(kv: (Key, List[Rule])): this.type = addRules(kv._2)
  def -=(key: Key): this.type = throw new NotImplementedError("cannot remove rule")
  def get(key: Key): Option[List[Rule]] = getRule(key._1, key._2)

  def hasRule(name: String, arity: Int): Boolean
  def getRule(name: String, arity: Int): Option[List[Rule]]
  def addRule(data: Rule): this.type
  def addRules(data: List[Rule]): this.type
}

class MapDatabase(_rules: List[Rule]) extends Database {
  private var rules: Map[Key, List[Rule]] = rulesListToMap(_rules)

  def hasRule(name: String, arity: Int): Boolean = rules.contains((name, arity))
  def getRule(name: String, arity: Int): Option[List[Rule]] = rules.get((name, arity))

  def iterator: Iterator[(Key, List[Rule])] = rules.iterator

  def addRule(rule: Rule): this.type = {
    val key = (rule.ruleName, rule.arity)
    if (rules.contains(key)) rules += key -> (rule :: rules(key))
    else rules += key -> List(rule)
    this
  }

  def addRules(rule: List[Rule]): this.type = {
    rules ++= rulesListToMap(rule)
    this
  }

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
