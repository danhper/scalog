package com.tuvistavie.scalog.models

object AstImplicits {
  implicit def stringToAtom(predicateName: String) = Atom(predicateName)

  implicit def stringToConstant(constantName: String) = Constant(constantName)
  implicit def stringToVariable(variableName: String) = Variable(variableName)

  implicit def stringToPredicate(predicateName: String) = Predicate(predicateName)

  implicit def atomToFormula(atom: Atom) = Formula(atom)
}
