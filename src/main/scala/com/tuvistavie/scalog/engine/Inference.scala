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

class Inference(val formula: Formula)(implicit database: Database) {
  if (formula.atoms isEmpty) throw new IllegalArgumentException("formula should not be empty")

  private var rules: List[Iterator[Rule]] = List(getRules(formula.atoms head))

  private var ruleIterator: Iterator[Rule] = nextIterator()

  def processNext(): InferenceResult = {
    if (ruleIterator.hasNext) {
      ruleIterator.next() match {
        case Fact(fact) => unify(formula.atoms.head, fact)
        case Clause(head, formula) => SimpleSuccess
      }
    } else if (!rules.isEmpty) {
      ruleIterator = nextIterator()
      processNext()
    } else {
      SimpleFailure
    }
  }

  private def nextIterator() = {
    val newRules = rules.head
    rules = rules.tail
    newRules
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

  private def getRules(atom: Atom): Iterator[Rule] = {
    database.getRule(atom.name, atom.arity) match {
      case Some(rules) => rules.iterator
      case None => throw new ExecutionException(s"no rule ${atom.name}/${atom.arity}")
    }
  }
}

object Inference {
  def apply(formula: Formula)(implicit database: Database): Inference = {
    new Inference(formula)
  }
}
