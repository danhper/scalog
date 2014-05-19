package com.tuvistavie.scalog.models

object AstImplicits {
  implicit def stringToAtom(predicateName: String) = Atom(predicateName)
  implicit def stringToConstant(constantName: String) = Constant(constantName)
  implicit def stringToVariable(variableName: String) = Variable(variableName)
}
