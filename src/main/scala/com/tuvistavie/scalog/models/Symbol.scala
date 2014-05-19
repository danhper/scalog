package com.tuvistavie.scalog.models

sealed trait Symbol extends NamedEntity

object Symbol {
  def apply(symbolName: String) = {
    if (symbolName.headOption.exists(c => c >= 'A' && c <= 'Z')) Variable(symbolName)
    else Constant(symbolName)
  }
}

case class Variable(name: String) extends Symbol {
  val regexp = Variable.regexp

  checkName(name)

  def v: Variable = this
}

object Variable {
  val regexp  = "[A-Z][a-z0-9_]*"
}

case class Constant(name: String) extends Symbol {
  val regexp = Constant.regexp

  checkName(name)

  def c: Constant = this
}

object Constant {
  val regexp  = "[a-z][a-z0-9_]*"
}
