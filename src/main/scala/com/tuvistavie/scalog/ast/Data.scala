package com.tuvistavie.scalog.ast

sealed trait Data

case class Clause(head: Atom, body: Formula) extends Data

case class Formula(atoms: List[Atom]) extends Data
