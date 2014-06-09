package com.tuvistavie.scalog.models

import scala.collection.mutable
import java.io.{FileNotFoundException, File}
import org.apache.commons.io.FilenameUtils
import com.tuvistavie.scalog.parsers.DatalogParser

trait Database {
  type Key = (String, Int)
  def rules: Map[Key, List[Rule]]

  def +=(rule: Rule): this.type = addRule(rule)
  def +=(kv: (Key, List[Rule])): this.type = addRules(kv._2)
  def -=(key: Key): this.type = throw new NotImplementedError("cannot remove rule")
  def get(key: Key): Option[List[Rule]] = getRule(key._1, key._2)
  def size: Int = rules.size

  def hasRule(name: String, arity: Int): Boolean
  def getRule(name: String, arity: Int): Option[List[Rule]]
  def addRule(data: Rule): this.type
  def addRules(data: List[Rule]): this.type
  def merge(database: Database): this.type
}

class MapDatabase(rulesList: List[Rule]) extends Database {
  private var _rules: Map[Key, List[Rule]] = rulesListToMap(rulesList)
  def rules: Map[Key, List[Rule]] = _rules

  def hasRule(name: String, arity: Int): Boolean = rules.contains((name, arity))
  def getRule(name: String, arity: Int): Option[List[Rule]] = rules.get((name, arity))

  def iterator: Iterator[(Key, List[Rule])] = rules.iterator

  def addRule(rule: Rule): this.type = {
    val key = (rule.ruleName, rule.arity)
    if (rules.contains(key)) _rules += key -> (rule :: rules(key))
    else _rules += key -> List(rule)
    this
  }

  def addRules(rule: List[Rule]): this.type = {
    _rules ++= rulesListToMap(rule)
    this
  }

  def merge(database: Database): this.type = {
    _rules ++= database.rules
    this
  }

  override def toString: String = _rules mkString "\n"

  private def rulesListToMap(rules: List[Rule]): Map[Key, List[Rule]] = {
    val map: mutable.Map[Key, List[Rule]] = mutable.Map.empty
    rules foreach { d =>
      val key = (d.ruleName, d.arity)
      if (map.contains(key)) map += key -> (d :: map(key))
      else map += key -> List(d)
    }
    map toMap
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MapDatabase]

  override def equals(other: Any): Boolean = other match {
    case that: MapDatabase =>
      (that canEqual this) &&
        _rules == that._rules
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(_rules)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Database {
  def apply(data: List[Rule]): Database = new MapDatabase(data)
  def empty: Database = new MapDatabase(List.empty)

  def fromModule(module: String)(implicit basePath: String): Database = {
    val dir = new File(basePath)
    val files = dir.list()
    files.find(FilenameUtils.removeExtension(_) == module) match {
      case Some(file) => DatalogParser.parseFile(file) match {
        case Left(database) => database
        case Right(error) => throw new RuntimeException(error)
      }
      case None => throw new FileNotFoundException(s"module $module not found")
    }
  }
}
