package com.tuvistavie.scalog.models

sealed trait Symbol extends NamedEntity {
  def name: String
}

case class Variable(name: String) extends Symbol {
  checkName(name)

  val regexp = Variable.regexp

  def v: Variable = this
}

object Variable {
  val regexp  = "[A-Z][a-z0-9_]*"
}

case class Constant(name: String) extends Symbol {
  checkName(name)

  val regexp = Constant.regexp

  def c: Constant = this
}

object Constant {
  val regexp  = "[a-z][a-z0-9_]*"
}
