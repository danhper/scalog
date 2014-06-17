package com.tuvistavie.scalog
package models

sealed trait UserInput

case class Import(module: String) extends UserInput

case class Query(formula: Formula, seenRules: List[Rule] = List.empty) extends UserInput {
  def atoms: List[Atom] = formula.atoms
}
