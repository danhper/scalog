package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models._
import com.tuvistavie.scalog.models.Clause
import com.tuvistavie.scalog.models.Fact

sealed trait InferenceResult

case class SuccessWith(atoms: List[Substitution]) extends InferenceResult
case object SimpleSuccess extends InferenceResult
case object SimpleFailure extends InferenceResult


class Substitution(val previous: Symbol, val next: Symbol) {
  override def toString: String = previous + " = " + next
}

class Inference(val atom: Atom, val rules: List[Rule])(implicit database: Database) {

  private val ruleIterator = rules.iterator

  def processNext(): InferenceResult = {
    if (!ruleIterator.hasNext) return SimpleFailure
    ruleIterator.next() match {
      case Fact(fact) => unify(atom, fact)
      case Clause(head, formula) => SimpleSuccess
    }
  }

  def unify(premise: Atom, fact: Atom): InferenceResult = {
    if (premise.arity != fact.arity || premise.name != fact.name) return SimpleFailure
    unifyArguments(premise.arguments, fact.arguments, List.empty)
  }

  private def unifyArguments(premiseSymbols: List[Symbol], factSymbols: List[Symbol], substitutions: List[Substitution]): InferenceResult = {
    (premiseSymbols, factSymbols) match {
      case (Nil, _) | (_, Nil) => if (substitutions.isEmpty) SimpleSuccess else SuccessWith(substitutions)
      case (premiseHead :: premiseTail, factHead :: factTail) => (premiseHead, factHead) match {
        case (c1: Constant, c2: Constant) => if (c1 == c2) unifyArguments(premiseTail, factTail, substitutions) else SimpleFailure
        case _ => SimpleSuccess
      }
    }
  }

}

object Inference {
  def apply(atom: Atom, rules: List[Rule])(implicit database: Database): Inference = {
    new Inference(atom, rules)
  }
}
