package com.tuvistavie.scalog
package ast

class Database(data: List[Data])
object Database {
  def apply(data: List[Data]) = new Database(data)
}

class Query(formula: Formula)
object Query {
  def apply(formula: Formula) = new Query(formula)
}

sealed trait Data

case class Clause(head: Atom, body: Formula) extends Data

case class Formula(atoms: List[Atom]) extends Data

class Atom(predicate: Predicate, arguments: List[Symbol])
object Atom {
  def apply(pred: Predicate, args: List[Symbol]) = new Atom(pred, args)
}

class Predicate(identifier: String)
object Predicate {
  def apply(id: String) = new Predicate(id)
}

sealed trait Symbol {
  def name: String
}

case class Variable(name: String) extends Symbol
case class Constant(name: String) extends Symbol
